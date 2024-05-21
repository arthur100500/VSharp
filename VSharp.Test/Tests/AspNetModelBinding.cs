
using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.AspNetCore.Mvc.ModelBinding.Binders;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    public class DogPhoto
    {
        public int DogPhotoId { get; set; }
        public string Url { get; set; }
        public string DogName { get; set; }
    }

    [TestSvmFixture]
    public static class AspNetModelBinding
    {
        // [TestSvm(expectedCoverage: 100)]
        [Ignore("TODO")]
        public static void ComplexTypeModelBinderBind()
        {
            // TODO: Create test
            // Constructors of the binders factories and model attributes are
            // very hard to grasp, so testing will wait a little
        }
    }
}