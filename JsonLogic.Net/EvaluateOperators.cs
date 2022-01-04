using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using Newtonsoft.Json.Linq;

namespace JsonLogic.Net
{
    public class EvaluateOperators : IManageOperators
    {
        private Dictionary<string, Func<IProcessJsonLogic, JToken[], object, object>> registry;

        public static EvaluateOperators Default { get; } = new EvaluateOperators();

        public EvaluateOperators()
        {
            registry = new Dictionary<string, Func<IProcessJsonLogic, JToken[], object, object>>();
            AddDefaultOperations();
        }

        public void AddOperator(string name, Func<IProcessJsonLogic, JToken[], object, object> operation)
        {
            registry[name] = operation;
        }

        public void DeleteOperator(string name)
        {
            registry.Remove(name);
        }

        public Func<IProcessJsonLogic, JToken[], object, object> GetOperator(string name)
        {
            return registry[name];
        }

        public static bool IsAny<T>(params object[] subjects)
        {
            return subjects.Any(x => x != null && x is T);
        }

        private void AddDefaultOperations()
        {
            AddOperator("==", (p, args, data) => p.Apply(args[0], data).EqualTo(p.Apply(args[1], data)));

            AddOperator("===", (p, args, data) => p.Apply(args[0], data).StrictEqualTo(p.Apply(args[1], data)));

            AddOperator("!=", (p, args, data) => !p.Apply(args[0], data).EqualTo(p.Apply(args[1], data)));

            AddOperator("!==", (p, args, data) => !p.Apply(args[0], data).StrictEqualTo(p.Apply(args[1], data)));

            AddOperator("+", (p, args, data) => Min2From(args.Select(a => p.Apply(a, data))).Aggregate((prev, next) =>
            {
                try
                {
                    return Convert.ToDouble(prev ?? 0d) + Convert.ToDouble(next);
                }
                catch
                {
                    return (prev ?? string.Empty).ToString() + next.ToString();
                }
            }));

            AddOperator("-", ReduceDoubleArgs(0d, (prev, next) => prev - next));

            AddOperator("/", ReduceDoubleArgs(1, (prev, next) => prev / next));

            AddOperator("*", ReduceDoubleArgs(1, (prev, next) => prev * next));

            AddOperator("%", ReduceDoubleArgs(1, (prev, next) => prev % next));

            AddOperator("max", (p, args, data) => args.Select(a => Convert.ToDouble(p.Apply(a, data)))
                .Aggregate((prev, next) => prev > next ? prev : next));

            AddOperator("min", (p, args, data) => args.Select(a => Convert.ToDouble(p.Apply(a, data)))
                .Aggregate((prev, next) => (prev < next) ? prev : next));

            AddOperator("<", GenericArgsSatisfy((prev, next) => prev < next, (prev, next) => string.CompareOrdinal(prev, next) < 0));

            AddOperator("<=", GenericArgsSatisfy((prev, next) => prev <= next, (prev, next) => string.CompareOrdinal(prev, next) <= 0));

            AddOperator(">", GenericArgsSatisfy((prev, next) => prev > next, (prev, next) => string.CompareOrdinal(prev, next) > 0));

            AddOperator(">=", GenericArgsSatisfy((prev, next) => prev >= next, (prev, next) => string.CompareOrdinal(prev, next) >= 0));

            AddOperator("var", (p, args, data) =>
            {
                if (args.Count() == 0) return data;

                var names = p.Apply(args.First(), data);
                if (names == null) return data;

                try
                {
                    var result = GetValueByName(data, names.ToString());
                    // This will return JValue or null if missing. Actual value of null will be wrapped in JToken with value null
                    if (result is JValue)
                    {
                        // permit correct type wrangling to occur (AdjustType) without duplicating code
                        result = p.Apply((JValue)result, null);
                    }
                    else if (result == null && args.Count() == 2)
                    {
                        object defaultValue = p.Apply(args.Last(), data);
                        result = defaultValue;
                    }

                    return result;
                }
                catch
                {
                    object defaultValue = (args.Count() == 2) ? p.Apply(args.Last(), data) : null;
                    return defaultValue;
                }
            });

            AddOperator("and", (p, args, data) =>
            {
                object value = p.Apply(args[0], data);
                for (var i = 1; i < args.Length && value.IsTruthy(); i++)
                {
                    value = p.Apply(args[i], data);
                }
                return value;
            });

            AddOperator("or", (p, args, data) =>
            {
                object value = p.Apply(args[0], data);
                for (var i = 1; i < args.Length && !value.IsTruthy(); i++)
                {
                    value = p.Apply(args[i], data);
                }
                return value;
            });

            AddOperator("!!", (p, args, data) => p.Apply(args.First(), data).IsTruthy());

            AddOperator("!", (p, args, data) => !p.Apply(args.First(), data).IsTruthy());

            AddOperator("not", GetOperator("!"));

            AddOperator("if", (p, args, data) =>
            {
                for (var i = 0; i < args.Length - 1; i += 2)
                {
                    if (p.Apply(args[i], data).IsTruthy()) return p.Apply(args[i + 1], data);
                }
                if (args.Length % 2 == 0) return null;
                return p.Apply(args[args.Length - 1], data);
            });

            AddOperator("?:", GetOperator("if"));

            AddOperator("missing", (p, args, data) =>
            {
                var names = args.Select(a => p.Apply(a, data));
                if (names.Count() == 1 && names.First().IsEnumerable()) names = names.First().MakeEnumerable();
                if (data == null) return names.ToArray();
                return names.Select(n => n.ToString()).Where(n =>
                {
                    try
                    {
                        GetValueByName(data, n);
                        return false;
                    }
                    catch
                    {
                        return true;
                    }
                }).ToArray();
            });

            AddOperator("missing_some", (p, args, data) =>
            {
                var minRequired = Convert.ToDouble(p.Apply(args[0], data));
                var keys = (args[1] as JArray).ToArray();
                var missingKeys = GetOperator("missing").Invoke(p, keys, data) as IEnumerable<object>;
                var validKeyCount = keys.Length - missingKeys.Count();
                return (validKeyCount >= minRequired) ? new object[0] : missingKeys;
            });

            AddOperator("map", (p, args, data) =>
            {
                object arr = p.Apply(args[0], data);
                if (arr == null) return new object[0];

                return arr.MakeEnumerable().Select(item => p.Apply(args[1], item)).ToArray();
            });

            AddOperator("filter", (p, args, data) =>
            {
                // if first part fails to retrieve data, make enumerable will fail
                IEnumerable<object> arr = p.Apply(args[0], data)?.MakeEnumerable();
                return arr?.Where(item => p.Apply(args[1], item).IsTruthy()).ToArray();
            });

            AddOperator("reduce", (p, args, data) =>
            {
                var initialValue = p.Apply(args[2], data);
                object arr = p.Apply(args[0], data);
                if (!arr.IsEnumerable()) return initialValue;

                return arr.MakeEnumerable().Aggregate(initialValue, (acc, current) =>
                {
                    object result = p.Apply(args[1], new { current = current, accumulator = acc });
                    return result;
                });
            });

            AddOperator("all", (p, args, data) =>
            {
                IEnumerable<object> arr = p.Apply(args[0], data).MakeEnumerable();
                if (arr.Count() == 0) return false;
                return arr.All(item => Convert.ToBoolean(p.Apply(args[1], item)));
            });

            AddOperator("none", (p, args, data) =>
            {
                IEnumerable<object> arr = p.Apply(args[0], data).MakeEnumerable();
                return !arr.Any(item => Convert.ToBoolean(p.Apply(args[1], item)));
            });

            AddOperator("some", (p, args, data) =>
            {
                IEnumerable<object> arr = p.Apply(args[0], data).MakeEnumerable();
                return arr.Any(item => Convert.ToBoolean(p.Apply(args[1], item)));
            });

            AddOperator("merge", (p, args, data) => args.Select(a => p.Apply(a, data)).Aggregate(new object[0], (acc, current) =>
            {
                try
                {
                    return acc.Concat(current.MakeEnumerable()).ToArray();
                }
                catch
                {
                    return acc.Concat(new object[] { current }).ToArray();
                }
            }));

            AddOperator("in", (p, args, data) =>
            {
                object needle = p.Apply(args[0], data);
                object haystack = p.Apply(args[1], data);
                if (haystack is null) return false;
                if (haystack is String) return (haystack as string).IndexOf(needle.ToString()) >= 0;

                return haystack.MakeEnumerable().Any(item => item.EqualTo(needle));
            });

            AddOperator("cat", (p, args, data) => args.Select(a => p.Apply(a, data)).Aggregate("", (acc, current) => acc + current.ToString()));

            AddOperator("substr", (p, args, data) =>
            {
                string value = p.Apply(args[0], data).ToString();
                int start = Convert.ToInt32(p.Apply(args[1], data));
                while (start < 0) start += value.Length;
                if (args.Count() == 2) return value.Substring(start);

                int length = Convert.ToInt32(p.Apply(args[2], data));
                if (length >= 0) return value.Substring(start, length);

                int end = length;
                while (end < 0) end += value.Length;
                return value.Substring(start, end - start);
            });

            AddOperator("log", (p, args, data) =>
            {
                object value = p.Apply(args[0], data);
                System.Diagnostics.Debug.WriteLine(value);
                return value;
            });

            AddOperator("within", (p, args, data) =>
            {
                double first = Convert.ToDouble(p.Apply(args[0], data));
                double second = Convert.ToDouble(p.Apply(args[1], data));
                double precision = Convert.ToDouble(p.Apply(args[2], data));
                return Math.Abs(first - second) <= precision;
            });

            // Local processing operator changes scope of evaluations of its
            // second argument to result of the first argument
            // i.e. "local": [ "sourceDataOrLogicAppliedToFullData", "logicToApplyToSourceOnly"]
            AddOperator("local", (p, args, data) =>
            {
                // if blank arguments, return null, since nothing matches conditions
                if (args.Length < 1) return null;
                var local = p.Apply(args.First(), data);
                if (args.Length >= 2) return p.Apply(args[1], local);
                else return local; // return result of source evaluation
            });

            AddOperator("plusTime", (p, args, data) =>
            {
                var dateTimeString = p.Apply(args[0], data);
                var amount = args[1];
                var timeUnit = (string)args[2];

                /**
                 * All time units as an array.
                 */
                var timeUnits = new string[] { "year", "month", "day", "hour" };

                if (!(amount is int))
                {
                    throw new Exception("'amount' argument (#2) of 'plusTime' must be an integer");
                }

                if (!timeUnits.Contains(timeUnit))
                {
                    throw new Exception($"'unit' argument (#3) of 'plusTime' must be a string with one of the time units: ${string.Join(", ", timeUnits)}");
                }

                if (!(dateTimeString == null || dateTimeString is string))
                {
                    throw new Exception("'date' argument (#1) of 'plusTime' must be a string");
                }

                return PlusTime((string)dateTimeString, (int)amount, timeUnit);
            });

            AddOperator("extractFromUVCI", (p, args, data) =>
            {
                var uvci = p.Apply(args[0], data);
                var index = args[1];

                if (!(uvci == null || uvci is string))
                {
                    throw new Exception("'UVCI' argument(#1) of 'extractFromUVCI' must be either a string or null");
                }

                if (!(index is int))
                {
                    throw new Exception("'index' argument(#2) of 'extractFromUVCI' must be an integer");
                }

                return ExtractFragmentFromUvci((string)uvci, (int)index);
            });
        }

        /// <summary>
        /// A date-time offset operation.
        ///
        /// A time unit is one of the following string values: "year", "month", "day", "hour".
        ///
        /// To convert a date(-time) string to a date-time value, specify an amount of 0, and any time unit.
        /// Note that plusTime does not permit other date-time values: expressions such as plusTime(plusTime("...", 0, "hour"), 10, "day") are not valid.
        /// </summary>
        /// <param name="dateTimeString">a date-time in the allowed string format</param>
        /// <param name="amount">the number of days/hours to add (may be negative)</param>
        /// <param name="timeUnit">string with a time unit</param>
        /// <returns>Modified date time</returns>
        private DateTime PlusTime(string dateTimeString, int amount, string timeUnit)
        {
            var dateTime = DateFromString(dateTimeString);

            if (amount == 0)
            {
                return dateTime;
            }

            if (timeUnit == "day")
            {
                return dateTime.AddDays(amount);
            }
            else if (timeUnit == "hour")
            {
                return dateTime.AddHours(amount);
            }
            else if (timeUnit == "month")
            {
                return dateTime.AddMonths(amount);
            }
            else if (timeUnit == "year")
            {
                var wasMonth = dateTime.Month;

                dateTime = dateTime.AddYears(amount);

                // don't know why this is needed yet
                if (dateTime.Month > wasMonth)
                {
                    dateTime = dateTime.AddDays(-1);
                }

                return dateTime;
            }
            else
            {
                throw new Exception($"unknown time unit '{timeUnit}'");
            }
        }

        private DateTime DateFromString(string dateTimeString)
        {
            return DateTime.Parse(dateTimeString);
            // todo swap this out with a method to dateFromString
            // export const dateFromString = (str: string) => {
        }

        private const string optionalPrefix = "URN:UVCI:";

        /// <summary>
        /// The fragment with given index from the UVCI string
        /// (see Annex 2 in the[UVCI specification](https://ec.europa.eu/health/sites/default/files/ehealth/docs/vaccination-proof_interoperability-guidelines_en.pdf)),
        /// or `null` when that fragment doesn't exist.
        /// </summary>
        private string ExtractFragmentFromUvci(string uvci, int index)
        {
            if (uvci == null || index < 0)
            {
                return null;
            }

            var prefixlessUvci = uvci.StartsWith(optionalPrefix) ? uvci.Substring(optionalPrefix.Length) : uvci;
            var fragments = Regex.Split(prefixlessUvci, "/[/#:]/");
            return index < fragments.Length ? fragments[index] : null;
        }

        private object GetValueByName(object data, string namePath)
        {
            if (string.IsNullOrEmpty(namePath)) return data;

            if (data == null) throw new ArgumentNullException(nameof(data));

            string[] names = namePath.Split('.');
            object d = data;
            foreach (string name in names)
            {
                if (d == null) return null;
                if (d.GetType().IsArray)
                {
                    d = (d as Array).GetValue(int.Parse(name));
                }
                else if (DictionaryType(d) != null)
                {
                    var type = DictionaryType(d);
                    var prop = type.GetTypeInfo().DeclaredProperties.FirstOrDefault(p => p.Name == "Item");
                    d = prop.GetValue(d, new object[] { name });
                }
                else if (d is IEnumerable<object>)
                {
                    d = (d as IEnumerable<object>).Skip(int.Parse(name)).First();
                }
                else
                {
                    var property = d.GetType().GetTypeInfo().GetDeclaredProperty(name);
                    if (property == null) throw new Exception();
                    d = property.GetValue(d);
                }
            }
            return d;
        }

        private Type DictionaryType(object d)
        {
            return d.GetType().GetTypeInfo().ImplementedInterfaces.FirstOrDefault(t => t.GetTypeInfo().IsGenericType && t.GetGenericTypeDefinition() == typeof(IDictionary<,>));
        }

        private Func<IProcessJsonLogic, JToken[], object, object> GenericArgsSatisfy(Func<Double, Double, bool> criteriaDouble, Func<string, string, bool> criteriaText)
        {
            return (p, args, data) =>
            {
                var values = args
                    .Select(a => p.Apply(a, data));

                var isAllText = values
                    .Where(a => a != null)
                    .Select(a => JToken.FromObject(a))
                    .All(a => a.Type == JTokenType.String);

                return isAllText ? CheckCriteria(values.Cast<string>().ToArray(), criteriaText)
                : CheckCriteria(values.Select(a => a == null ? 0d : Double.Parse(a.ToString())).ToArray(), criteriaDouble);
            };
        }

        private bool CheckCriteria<T>(T[] values, Func<T, T, bool> criteria)
        {
            for (int i = 1; i < values.Length; i++)
            {
                if (!criteria(values[i - 1], values[i])) return false;
            }

            return true;
        }

        private static Func<IProcessJsonLogic, JToken[], object, object> ReduceDoubleArgs(double defaultValue, Func<double, double, double> reducer)
        {
            return (p, args, data) => Min2From(args.Select(a => p.Apply(a, data))).Select(a => a == null ? defaultValue : Convert.ToDouble(a)).Aggregate(reducer);
        }

        private static IEnumerable<object> Min2From(IEnumerable<object> source)
        {
            var count = source.Count();
            if (count >= 2) return source;

            return new object[] { null, count == 0 ? null : source.First() };
        }
    }
}
