using System;
using System.IO;
using System.Text.Json;
using System.Threading;
using NUnit.Framework;
using VSharp.Test;
using System.Text.Json;

namespace IntegrationTests;

public class Wallet
{
    public int MoneyAmount { get; set; }

    public void SetMoney(int newAmount)
    {
        MoneyAmount = newAmount;
    }
}

[TestSvmFixture]
public class JsonTests
{
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

    [TestSvm(expectedCoverage: 100)]
    public static int JsonDeserializeReferenceType(Wallet arg)
    {
        var memoryStream = new MemoryStream();
        JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
        memoryStream.Position = 0;
        var result = (Wallet)JsonSerializer.DeserializeAsync(memoryStream, typeof(Wallet)).Result!;
        return result.MoneyAmount + 1;
    }

    [TestSvm(expectedCoverage: 100)]
    public static int JsonDeserializeValueType(int arg)
    {
        var memoryStream = new MemoryStream();
        JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
        memoryStream.Position = 0;
        var result = (int)JsonSerializer.DeserializeAsync(memoryStream, typeof(int)).Result!;
        return result + 2;
    }
}