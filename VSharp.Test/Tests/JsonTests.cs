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

public class DogPhoto
{
    public int DogPhotoId { get; set; }
    public string Url { get; set; }
    public string DogName { get; set; }
}


public class RecursiveBox
{
    public RecursiveBox Self;
    public int Field;

    public RecursiveBox(int field)
    {
        Self = this;
        Field = field;
    }
}


/// <summary>
/// More than tests for serialization and deserialization
/// These test collection tests Object.DeepCopy
/// </summary>
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

    [TestSvm(expectedCoverage: 100)]
    public static int JsonCopiesObject(Wallet arg)
    {
        var memoryStream = new MemoryStream();
        JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
        memoryStream.Position = 0;
        var first = (Wallet)JsonSerializer.DeserializeAsync(memoryStream, typeof(Wallet)).Result!;
        first.MoneyAmount += 10;
        memoryStream.Position = 0;
        var second = (Wallet)JsonSerializer.DeserializeAsync(memoryStream, typeof(Wallet)).Result!;
        second.MoneyAmount += 30;
        return first.MoneyAmount + second.MoneyAmount;
    }

    [TestSvm(expectedCoverage: 100)]
    public static int JsonCopiesObject2(DogPhoto arg)
    {
        var memoryStream = new MemoryStream();
        JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
        memoryStream.Position = 0;
        var first = (DogPhoto)JsonSerializer.DeserializeAsync(memoryStream, typeof(DogPhoto)).Result!;
        first.DogPhotoId += 10;
        memoryStream.Position = 0;
        var second = (DogPhoto)JsonSerializer.DeserializeAsync(memoryStream, typeof(DogPhoto)).Result!;
        second.DogPhotoId += 30;
        return first.DogPhotoId + second.DogPhotoId;
    }

    [Ignore("Deep copying recursive object")]
    public static int JsonCopyManagesRecursiveTypes()
    {
        var box = new RecursiveBox(3241);
        var memoryStream = new MemoryStream();
        JsonSerializer.SerializeAsync(memoryStream, box).Wait();
        memoryStream.Position = 0;
        var passedBox = (RecursiveBox)JsonSerializer.DeserializeAsync(memoryStream, typeof(RecursiveBox)).Result!;
        return passedBox.Field;
    }

    [TestSvm(expectedCoverage: 100)]
    public static string JsonCopiesCorrectString(DogPhoto arg)
    {
        if (arg.DogName == "") throw new ArgumentException("Dog name must not be empty");
        var memoryStream = new MemoryStream();
        JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
        memoryStream.Position = 0;
        var first = (DogPhoto)JsonSerializer.DeserializeAsync(memoryStream, typeof(DogPhoto)).Result!;
        first.DogName += " 3241 ";
        memoryStream.Position = 0;
        var second = (DogPhoto)JsonSerializer.DeserializeAsync(memoryStream, typeof(DogPhoto)).Result!;
        second.DogName = "340597" + second.DogName;
        return first.DogName + second.DogName;
    }
}
