﻿using System;
using System.Diagnostics.CodeAnalysis;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public class Attributes
    {
        [TestSvm]
        public int DisallowNullTest1([DisallowNull] object obj)
        {
            if (obj == null)
            {
                throw new NullReferenceException();
            }
            return 1;
        }

        [TestSvm]
        public int DisallowNullTest2([DisallowNull] object obj)
        {
            if (obj != null)
            {
                return 1;
            }
            return 0;
        }

        [TestSvm]
        public int DisallowNullTest3([DisallowNull] Typecast.Piece piece, int n)
        {
            if (n == 42)
            {
                return piece.GetRate();
            }
            if (n == 43)
            {
                return piece.GetRate() + n;
            }
            return 1;
        }

        [TestSvm]
        public int NotNullTest1([NotNull] object obj)
        {
            if (obj == null)
            {
                throw new NullReferenceException();
            }
            return 1;
        }

        [TestSvm]
        public int NotNullTest2([NotNull] object obj)
        {
            if (obj != null)
            {
                return 1;
            }
            return 0;
        }

        [TestSvm]
        public int NotNullTest3([NotNull] object obj)
        {
            return 1;
        }

        [TestSvm]
        public int NotNullCallsDisallowNullTest1([NotNull] object obj)
        {
            return DisallowNullTest1(obj);
        }

        [TestSvm]
        public int NotNullCallsDisallowNullTest2([NotNull] object obj)
        {
            if (obj == null)
            {
                return DisallowNullTest1(obj);
            }
            return 1;
        }

        [TestSvm]
        public int DisallowNullCallsNotNullTest([DisallowNull] object obj)
        {
            return NotNullTest1(obj);
        }
    }
}
