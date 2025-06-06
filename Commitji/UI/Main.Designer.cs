namespace Commitji.UI;

partial class Main
{
    /// <summary>
    ///  Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    ///  Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing)
    {
        if (disposing && (components != null))
        {
            components.Dispose();
        }

        base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Main));
        inputTextBox = new System.Windows.Forms.TextBox();
        selectionLabel = new System.Windows.Forms.Label();
        hintTextBox = new System.Windows.Forms.TextBox();
        SuspendLayout();
        // 
        // inputTextBox
        // 
        inputTextBox.Dock = System.Windows.Forms.DockStyle.Top;
        inputTextBox.Font = new System.Drawing.Font("Lucida Console", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte) 0));
        inputTextBox.Location = new System.Drawing.Point(10, 11);
        inputTextBox.Name = "inputTextBox";
        inputTextBox.Size = new System.Drawing.Size(780, 22);
        inputTextBox.TabIndex = 0;
        // 
        // selectionLabel
        // 
        selectionLabel.Dock = System.Windows.Forms.DockStyle.Top;
        selectionLabel.Font = new System.Drawing.Font("Lucida Console", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte) 0));
        selectionLabel.Location = new System.Drawing.Point(10, 33);
        selectionLabel.Name = "selectionLabel";
        selectionLabel.Padding = new System.Windows.Forms.Padding(0, 16, 0, 16);
        selectionLabel.Size = new System.Drawing.Size(780, 57);
        selectionLabel.TabIndex = 1;
        selectionLabel.Text = "Selection";
        // 
        // hintTextBox
        // 
        hintTextBox.BackColor = System.Drawing.SystemColors.Info;
        hintTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
        hintTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
        hintTextBox.Font = new System.Drawing.Font("Lucida Console", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte) 0));
        hintTextBox.ForeColor = System.Drawing.SystemColors.InfoText;
        hintTextBox.Location = new System.Drawing.Point(10, 90);
        hintTextBox.Multiline = true;
        hintTextBox.Name = "hintTextBox";
        hintTextBox.ReadOnly = true;
        hintTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
        hintTextBox.Size = new System.Drawing.Size(780, 641);
        hintTextBox.TabIndex = 3;
        hintTextBox.Text = "💡 Hint";
        // 
        // Main
        // 
        AutoScaleDimensions = new System.Drawing.SizeF(7F, 17F);
        AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        ClientSize = new System.Drawing.Size(800, 742);
        Controls.Add(hintTextBox);
        Controls.Add(selectionLabel);
        Controls.Add(inputTextBox);
        Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte) 0));
        Icon = ((System.Drawing.Icon) resources.GetObject("$this.Icon"));
        Padding = new System.Windows.Forms.Padding(10, 11, 10, 11);
        Text = "Commitji";
        TopMost = true;
        ResumeLayout(false);
        PerformLayout();
    }

    private System.Windows.Forms.TextBox hintTextBox;

    private System.Windows.Forms.Label selectionLabel;

    private System.Windows.Forms.TextBox inputTextBox;

    #endregion
}