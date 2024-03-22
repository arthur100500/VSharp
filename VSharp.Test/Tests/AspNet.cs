using System;
using System.IO;
using System.Text.Json;
using System.Threading;
using VSharp.Test;
using System.Text.Json;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class AspNet
    {
        public class Wallet
        {
            public int MoneyAmount { get; set; }
        }

        [TestSvm(100)]
        public static int JsonSerialize(Wallet symbolicObject)
        {
            var memoryStream = new MemoryStream();

            JsonSerializer.SerializeAsync(memoryStream, symbolicObject).Wait();
            memoryStream.Seek(0, SeekOrigin.Begin);

            var deserializedObject = JsonSerializer.DeserializeAsync(
                memoryStream,
                typeof(Wallet),
                JsonSerializerOptions.Default,
                CancellationToken.None
            );

            var amount = ((Wallet)deserializedObject.Result!).MoneyAmount;

            if (amount < 0)
                throw new ArgumentException(nameof(amount));

            return amount;
        }
    }
}
