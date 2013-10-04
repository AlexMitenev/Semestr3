using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace IsRightTrangles
{
    public partial class Drawing : Form
    {
        int x1, y1, x2, y2, x3, y3;

        public Drawing()
        {
            InitializeComponent();
        }

        private void DrawIt_Click(object sender, EventArgs e)
        {
            x1 = Convert.ToInt32(textX1.Text);
            y1 = Convert.ToInt32(textY1.Text);
            x2 = Convert.ToInt32(textX2.Text);
            y2 = Convert.ToInt32(textY2.Text);
            x3 = Convert.ToInt32(textX3.Text);
            y3 = Convert.ToInt32(textY3.Text);

            pictureBox1.Refresh();
        }

        private void pictureBox1_Paint(object sender, PaintEventArgs e)
        {
            Point p1 = new Point(x1, y1);
            Point p2 = new Point(x2, y2);
            Point p3 = new Point(x3, y3);
            Pen pen = new Pen(Color.Black);
            e.Graphics.DrawLine(pen, p1, p2);
            e.Graphics.DrawLine(pen, p2, p3);
            e.Graphics.DrawLine(pen, p3, p1);

            textValue.Text = Lib.RightTrangle.IsRightTrangle(x1, y1, x2, y2, x3, y3).ToString();
        }

    }
}
