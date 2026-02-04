# Dragon Kill Points (DKP) - Coauthor Collaboration Tracker

A Shiny app for tracking and managing author contributions to collaborative research projects with automatic authorship list generation. Project inspired by the framework developped by Martinig et al. (2025) "Dragon Kill Points: applying a transparent working template to relieve authorship stress". DOI: https://doi.org/10.32942/X2W05K

## Overview

This application helps research teams:
- Track individual contributions across multiple project tasks
- Automatically generate fair authorship lists based on contribution scores
- Handle special cases (senior authors, equal contributions, exclusions)
- Save and restore project states via XML

### R Packages required
- `shiny`
- `DT`
- `xml2`
- `dplyr`
- `tidyr`

Install with:
```r
install.packages(c("shiny", "DT", "xml2", "dplyr", "tidyr"))
```

### Input Files

The app requires 3 files and these files are sufficient to run it in any new project:

1. `app.R` - the main Shiny app file
2. `names.txt` - list of author names
3. `tasks.txt` - list of tasks with types

#### 1. `names.txt`
Contains author names, one per line.

Example:
```
Szymon Drobniak
John Smith
Jane Doe
```

#### 2. `tasks.txt`
Contains tasks in format: `TaskGroup:Task:Type`

- **TaskGroup**: Category of the task (currently the app supports a fixed set: Conceptualisation, Investigation, Validation, Analysis, Writing, Revision, Other)
- **Task**: Specific task name
- **Type**: Either `Binary` (0/1) or `Quant` (arbitrary numbers)

Example:
```
Conceptualisation:Initial discussions:Binary
Conceptualisation:Idea refinment meetings:Binary
Investigation:Field work:Quant
Writing:First draft:Binary
Writing:Revisions:Quant
```

Quantitative scores are automatically normalised (they are displayed as numbers but internally they are scaled to [0,1] interval).

## Getting Started

1. Ensure `names.txt` and `tasks` files exist in the app directory
2. Run the app:
   ```r
   shiny::runApp('path')
   ```
   Or open `app.R` in RStudio and click "Run App"

3. The app will load and display an editable spreadsheet

The path argument is optional, it's the relative path to the subfolder with the app file, can be omitted if you are already in that folder.

## Using the App

### Main Spreadsheet

The spreadsheet shows:
- **Rows**: Authors (from `names.txt`)
- **Columns**: Tasks grouped by task groups (from `tasks.txt` file)
- **Special columns**:
  - `Senior`: Mark as 1 to place author at end of list with asterisk
  - `Equal`: Use same number to group authors with equal contribution
  - `Exclude`: Mark as 1 to exclude author from final list
  - `Total`: Calculated sum (not editable)

### Adding Points

1. Click any cell in the spreadsheet
2. Enter a value:
   - For **Binary** tasks: Enter `0` or `1`
   - For **Quant** tasks: Enter any number (e.g., hours worked, percentage contribution)
3. Empty cells count as `0`

### Adding New Tasks

1. In the "Add Task" field, enter: `TaskGroup:Task:Type`
   - Example: `Analysis:Statistical modeling:Quant`
2. Click "Add Task"
3. New column appears in spreadsheet
4. Task is automatically saved to `tasks.txt` file

### Adding New Authors

1. In the "Add Author" field, enter the author's name
2. Click "Add Author"
3. New row appears with empty cells
4. Name is automatically saved to `names.txt`

### Calculating Totals

1. Click "Calculate Totals" button
2. The app calculates scores:
   - **Binary tasks**: Scores are summed directly (0 or 1)
   - **Quant tasks**: Scores are normalized (your value / sum of all values for that task)
3. Total scores appear in the `Total` column
4. Author list is generated below the table

### Author List Rules

The generated list follows these rules:

1. **Threshold**: If set, only authors with `Total >= threshold` are included
2. **Excluded authors**: Those with `Exclude = 1` are not listed
3. **Main authors**: Sorted by Total score (descending)
4. **Equal contributions**: Authors with same `Equal` value are grouped:
   - Positioned based on highest score in group
   - Listed alphabetically by surname
   - Marked with superscript `^#`
5. **Senior authors**: Those with `Senior = 1`:
   - Placed at the end regardless of score
   - Listed alphabetically by surname
   - Marked with asterisk `*`

### Example Author List Output

```
Jane Doe, John Smith^#, Alice Brown^#, Robert Lee, Szymon Drobniak*
```

Meaning:
- Jane Doe has the highest score
- John Smith and Alice Brown have equal contributions (same `Equal` value)
- Robert Lee follows
- Szymon Drobniak is a senior author

### Saving and Loading State

**To Save:**
1. Enter filename in "XML Filename" field (e.g., `project_jan2025.xml`)
2. Click "Save to XML"
3. All data (points, tasks, authors) is saved

**To Load:**
1. Enter filename of existing XML file
2. Click "Load from XML"
3. Previous state is restored
4. If `names.txt` or `tasks.txt` files are missing, they're recreated from XML

## Example Workflow

If using the app in a new project - you can place the app files inside your project's folder (or subfolder). Then:

1. Create/update `names.txt` with your team members
2. Create/update `tasks.txt` file with your project tasks (or use one of the templates)
3. Start the app
4. Enter contribution scores for each person and task
5. Mark senior authors (e.g., PI, advisors)
6. Set threshold if needed (e.g., 2.0 for significant contribution)
7. Click "Calculate Totals"
8. Review generated author list
9. Save to XML (e.g., `manuscript_v1.xml`)
10. Make adjustments and recalculate as needed
11. Use final author list in your manuscript
12. Export CSV for reproducibility and reporting purposes

## File Structure

```
dragonkillpoints_app/
├── app.R              # Main Shiny application
├── names.txt          # List of authors (required)
├── tasks.txt         # List of tasks (required)
├── *.xml             # Saved states (optional)
└── README.md         # This file
```

## License

Open source - use freely for academic collaboration tracking.
