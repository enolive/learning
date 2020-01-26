using System;
using System.Net.Http;
using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;
using Newtonsoft.Json;

namespace ReactiveWebClient
{
    internal static class Program
    {
        private static void Main()
        {
            const string uri = "http://api.icndb.com/jokes/random";
            var httpClient = new HttpClient();
            var jokeText = Observable
                .FromAsync(() => httpClient.GetAsync(uri))
                .SelectMany(message => message.Content.ReadAsStringAsync())
                .Select(ToJson)
                .Select(result => result.Content.Text)
                .ToTask()
                .Result;

            Console.WriteLine(jokeText);
        }

        private static JokeResult ToJson(string content) => JsonConvert.DeserializeObject<JokeResult>(content);

        private class JokeResult
        {
            [JsonProperty("value")]
            public JokeContent Content { get; set; }
        }

        private class JokeContent
        {
            [JsonProperty("joke")]
            public string Text { get; set; }
        }
    }
}
