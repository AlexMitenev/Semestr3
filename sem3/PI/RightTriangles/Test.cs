using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;

namespace GraphicLibruary
{
    class Test
    {
        [Test]
        public void RightTriangle()
        {
            Assert.AreEqual(true, Lib.RightTrangle.IsRightTriangle(0, 0, 3, 0, 0, 4));
        }

        [Test]
        public void NotRightTriangle()
        {
            Assert.AreEqual(false, Lib.RightTrangle.IsRightTriangle(0, 0, 2, 0, 0, 4));
        }

        [Test]
        public void NotTriangle()
        {
            Assert.AreEqual(false, Lib.RightTrangle.IsTriangle(1, 3, 6));
        }
    }
}
