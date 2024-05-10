using System;
using System.Threading.Tasks;
using VSharp.Test;

namespace IntegrationTests;

[TestSvmFixture]
public class Tasks
{
    public class TwoBox
    {
        public int Id;
        public string Contents { get; set; }

        public void CheckCorrectness()
        {
            if (Id < 0) throw new ArgumentException("Box ID must be greater than 0");
        }
    }

    [TestSvm]
    public static TwoBox TaskFromResult(TwoBox box)
    {
        var task = Task.FromResult(box);
        task.Wait();
        task.Result.CheckCorrectness();
        return task.Result;
    }

    [TestSvm]
    public static void TaskFromException(TwoBox box)
    {
        var task = Task.FromException(new NullReferenceException("Artificial exception"));
        task.Wait();
    }
}