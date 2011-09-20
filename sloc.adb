-- sloc:  Source Code Counters

-- @Author    Matt Emborsky
-- @Version   0.8
-- @Date      March 5, 2007

--  This program will take any paramter and assume that it is an ada source code file.
--  This paramater could be a list of paramaters. During execution the program will open
--  each file individually and add up each line as one of the following:
--    Code
--    Comment
--    Count - Total line count
--    Null - No Code or Comment on this line
--  After going through all lines in the first file, the details are output to the screen.
--  After that we will add those counts to the total count and repeat until we have gone
--  through all the counts in the parameter list. We will then output those totals to the
--  screen and exit.
With Ada.Text_IO;
With Ada.Integer_Text_IO;
With Ada.Command_Line;

-- This is a comment to add to the counter for testing.
Procedure sloc is

  -- Handles the files for reading.
  File_Handle : Ada.Text_IO.File_Type;

  -- All the counters we use in calculating the lines of code for given file.
  Line_Count        : Integer := 0;
  Code_Count        : Integer := 0;
  Comment_Count     : Integer := 0;
  Null_Count        : Integer := 0;

  -- Total counters for keeping total of all given files.
  Total_Line_Count    : Integer := 0;
  Total_Comment_Count : Integer := 0;
  Total_Code_Count    : Integer := 0;
  Total_Null_Count    : Integer := 0;

  -- Skip_White_Space - Skip the White Space
  -- @Line: String of Line data
  -- @Index: Start of Index offsets
  --
  -- This will skip the white space from the Index offset to the first non-spacial
  -- character of either ' ' or <tab>. Since this is a procedure, we will be able to
  -- update the live value that is passed in.
  Procedure Skip_White_Space (Line : String; Offset : IN OUT Integer) is

  Begin

    While Line(Offset) = ' ' or Character'Pos(Line(Offset)) = 9 Loop

      Offset := Offset + 1;

    End Loop;

  End Skip_White_Space;

Begin

  -- Check to see if we have an argument. If we don't have atleast one argument, then we
  -- will skip the rest of the program and quit. The usage will be put if no argument is
  -- given.
  If Ada.Command_Line.Argument_Count < 1 Then

    Ada.Text_IO.Put_Line("Command Usage: ./sloc <file 1> <file 2> ...");

  Else

    -- Loop for each paramater that is passed in.
    For Current_Argument_Count in 1..Ada.Command_Line.Argument_Count Loop

      -- Reset our file variables for what our current counts are.
      Line_Count      := 0;
      Code_Count      := 0;
      Comment_Count   := 0;
      Null_Count      := 0;

      -- Open the current file for reading.
      Ada.Text_IO.Open(File_Handle, Ada.Text_IO.IN_File, Ada.Command_Line.Argument(Current_Argument_Count));

      -- Loop while we aren't at the end of the file.
      While Not Ada.Text_IO.End_Of_File(File_Handle) Loop

        -- Increase the size of our line counter to have an accurate reading of how
        -- many lines are in the file.
        Line_Count := Line_Count + 1;

        Declare

          -- Grab our line and store it in Line_String. The set the string Index to 1.
          --
          -- This method is to get away from initializing the string at the top of the
          -- program so we can get a dynamic string length and still be able to manipulate
          -- the string.
          Line_String : String := Ada.Text_IO.Get_Line(File_Handle);
          Index       : Integer := 1;

        Begin

          Begin

            -- Used to skip over the white space in front of the actual line data so we can
            -- see if the line is a comment or a code line.
            Skip_White_Space(Line_String, Index);

            -- If the first two true characters in the line are -- then the line is a
            -- a comment line, else the line is a code line, even if it ends with a
            -- comment we count it as a code line.
            If Line_String(Index .. Index + 1) = "--" Then

              Comment_Count := Comment_Count + 1;

            Else

              Code_Count := Code_Count + 1;

            End If;

          Exception

            When CONSTRAINT_ERROR =>

              -- If we have made it here, then we have an error in the Skip_White_Space
              -- proceudre and we have a null line. This is to get around the fact that
              -- a line could be either null of space, or have spaces but no text in it.
              Null_Count := Null_Count + 1;

          End;

        End;

      End Loop;

      -- Close the file so we can resuse variables. This also will stop memory leaks for
      -- large file sets.
      Ada.Text_IO.Close(File_Handle);

      -- Begin Data Output for file counts.
      Ada.Text_IO.Put_Line("File Name: " & Ada.Command_Line.Argument(Current_Argument_Count));

      Ada.Text_IO.Put("Code Line Count:    ");
      Ada.Integer_Text_IO.Put(Code_Count, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Comment Line Count: ");
      Ada.Integer_Text_IO.Put(Comment_Count, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Null Line Count:    ");
      Ada.Integer_Text_IO.Put(Null_Count, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("File Line Count:    ");
      Ada.Integer_Text_IO.Put(Line_Count, 0);
      Ada.Text_IO.New_Line(2);
      -- End Data Output for file counts.

      -- Add to totals the current file counts.
      Total_Line_Count    := Total_Line_Count + Line_Count;
      Total_Comment_Count := Total_Comment_Count + Comment_Count;
      Total_Null_Count    := Total_Null_Count + Null_Count;
      Total_Code_Count    := Total_Code_Count + Code_Count;

    End Loop;

    -- Begin Data Output for total counts.
    Ada.Text_IO.Put("Total Code Count:    ");
    Ada.Integer_Text_IO.Put(Total_Code_Count, 0);
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put("Total Comment Count: ");
    Ada.Integer_Text_IO.Put(Total_Comment_Count, 0);
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put("Total Null Count:    ");
    Ada.Integer_Text_IO.Put(Total_Null_Count, 0);
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put("Total Line Count:    ");
    Ada.Integer_Text_IO.Put(Total_Line_Count, 0);
    Ada.Text_IO.New_Line(2);
    -- End Data Output for total counts.

  End If;

End sloc;
