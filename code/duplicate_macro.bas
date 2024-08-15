Attribute VB_Name = "Module1"
Sub DuplicateIfEmptyOrMoveDownToEnd()
    Dim currentCell As Range
    
    ' Set initial cell to active cell
    Set currentCell = ActiveCell
    
    ' Loop until the end of the worksheet is reached
    Do Until currentCell.Row > Cells(Rows.Count, currentCell.Column).End(xlUp).Row
        ' If the current cell is empty, duplicate the above cell's value
        If currentCell.Value = "" Then
            currentCell.Value = currentCell.Offset(-1, 0).Value
        End If
        
        ' Move the cursor down
        Set currentCell = currentCell.Offset(1, 0)
    Loop
End Sub

