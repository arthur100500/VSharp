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
            public int MoneyAmount = 0;
        }

        [TestSvm(100)]
        public static int JsonSerialize(Wallet symbolicObject)
        {
            var serializedObject = JsonSerializer.Serialize(symbolicObject);
            var memoryStream = new MemoryStream();
            memoryStream.WriteByte((byte)serializedObject[0]);
            var deserializedObject = JsonSerializer.DeserializeAsync(memoryStream, typeof(Wallet),
                JsonSerializerOptions.Default, CancellationToken.None);

            var amount = ((Wallet)deserializedObject.Result!).MoneyAmount;
            return amount;
        }
    }
}
