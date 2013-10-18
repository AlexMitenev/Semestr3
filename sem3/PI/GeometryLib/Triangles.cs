using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lib
{
    public class RightTrangle
    {
        public static bool IsRightTriangle(int x1, int y1, int x2, int y2, int x3, int y3)
        {
            int len1 = (int)Math.Sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
            int len2 = (int)Math.Sqrt((x2 - x3) * (x2 - x3) + (y2 - y3) * (y2 - y3));
            int len3 = (int)Math.Sqrt((x3 - x1) * (x3 - x1) + (y3 - y1) * (y3 - y1));

            if (!IsTriangle(len1, len2, len3)) return false;

            return ((len1 * len1 + len2 * len2 == len3 * len3) |
                    (len2 * len2 + len3 * len3 == len1 * len1) |
                    (len3 * len3 + len1 * len1 == len2 * len2));
        }

        public static bool IsTriangle(int len1, int len2, int len3)
        {
            return !((len1 + len2 < len3) |
                     (len2 + len3 < len1) |
                     (len3 + len1 < len2));
        }
    }
}

