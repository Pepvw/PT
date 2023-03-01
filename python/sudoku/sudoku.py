# Pepijn van Wijk
# 13952072
# Bachelor informatica
#
# This program solves a n x n sudoku, with n being a square integer. The
# program works by calling the program and the path to the sudoku that is to be
# solved. If there is no solution possible this is printed. It is also possible
# to get multiple solutions for a sudoku, this is done by writing the number
# of solutiions after the path of the sudoku. If there are less possible
# solutions that given, all possible solutions are printed.

import math
import sys
import copy


class Sudoku():
    def __init__(self, filename):
        self.sudoku = self.load_sudoku(filename)
        self.size = len(self.sudoku)
        self.grid_size = int(math.sqrt(self.size))
        self.values = [i + 1 for i in range(self.size)]

    # Converts a file to a 2d list of integers, with every space seperated
    # integer in the file corresponding to an element in a row, and the lines
    # in the file corresponding to the rows of the 2d list.
    #
    # Input: A file name including the path.
    # Output: A list of lists containing each integer fromt the file.
    def load_sudoku(_, filename):
        list = []
        with open(filename) as f:
            for line in f:
                line = [int(i) for i in line.split()]
                list.append(line)
        return list

    # Gets the given row from the sudoku.
    #
    # Input: The row to be returned.
    # Output: the row of the matrix.
    def get_row(self, row):
        return self.sudoku[row]

    # Gets the given column from the sudoku.
    #
    # Input: The column to be returned.
    # Output: A list of the values of the column of the matrix.
    def get_col(self, col):
        return [row[col] for row in self.sudoku]

    # Gets the subgrid of the given coordinates of the sudoku.
    #
    # Input: The coordinates of the cell from the box that is  to
    # be returned.
    # Output: A list with the values in the box of the cell.
    def get_subgrid(self, row, col):
        block = []
        rows = self.sudoku[
                        ((row // self.grid_size) * self.grid_size):
                        ((((row // self.grid_size) * self.grid_size))
                         + self.grid_size)]
        for row in rows:
            block = block + row[
                                ((col // self.grid_size) * self.grid_size):
                                ((((col // self.grid_size) * self.grid_size))
                                 + self.grid_size)]
        return block

    # Returns a list of tuples with each tuple being in the form:
    # (row, col ,[possible values])
    #
    # Output: A list with tuples with every possible values for each open spot.
    def get_open_cells(self):
        list = []
        for row in range(self.size):
            for col in range(self.size):
                if self.sudoku[row][col] == 0:
                    list.append((row, col, self.free_at_pos(row, col)))
        list.sort(key=lambda a: len(a[2]))
        return list

    # Gets the values that are not yet in the given row of the sudoku.
    #
    # Input: The row.
    # Output: A list of the values that are not yet in the given row of the
    # matrix.
    def free_in_row(self, row):
        return [value for value in self.values
                if value not in self.get_row(row)]

    # Gets the values that are not yet in the given column of the sudoku.
    #
    # Input: The column.
    # Output: A list of the values that are not yet in the given column of the
    # matrix.
    def free_in_col(self, col):
        return [value for value in self.values
                if value not in self.get_col(col)]

    # Gets the values that are not yet in the subgrid of a given cell in the
    # matrix.
    #
    # Input: The coordinates of the cell.
    # Output: A list of the values that are not yet in the box of the cell of
    # the matrix.
    def free_in_subgrid(self, row, col):
        return [value for value in self.values
                if value not in self.get_subgrid(row, col)]

    # Gets the values that are possible on the coordinates of the given cell.
    #
    # Input: The coordinates of a cell in the sudoku.
    # Output: A list with every possible value in this cell of the sudoku.
    def free_at_pos(self, row, col):
        free_set = set(self.free_in_row(row)) & set(self.free_in_col(col))
        free_set = free_set & set(self.free_in_subgrid(row, col))
        return list(free_set)

    # Checks whether the row is valid and follows the sudoku rules.
    #
    # Input: The row to be checked.
    # Output True if valid, false otherwise.
    def row_valid(self, row):
        return (sum(self.get_row(row)) == sum(set(self.get_row(row))))

    # Checks whether the column is valid and follows the sudoku rules.
    #
    # Input: The column to be checked.
    # Output True if valid, false otherwise.
    def col_valid(self, col):
        return (sum(self.get_col(col)) == sum(set(self.get_col(col))))

    # Checks whether the subgrid of a given cell is valid and follows the
    # sudoku rules.
    #
    # Input: The coordinates of the cell that is to be checked.
    # Output True if valid, false otherwise.
    def subgrid_valid(self, row, col):
        return (sum(self.get_subgrid(row, col)) ==
                sum(set(self.get_subgrid(row, col))))

    # Checks whether the sudoku is valid and follows the sudoku rules by going
    # over every row, column and subgrid.
    #
    # Output: True if valid, false otherwise.
    def consistent(self):
        list = []
        for row in range(0, self.size, self.grid_size):
            for col in range(0, self.size, self.grid_size):
                list.append(self.subgrid_valid(row, col))
        list.append([self.row_valid(row) for row in range(self.size)])
        list.append([self.col_valid(col) for col in range(self.size)])
        return all(list)

    # Sets the value of a given cell to the value given.
    #
    # Input: Row, col and value of the cell to be set.
    def set_cell(self, row, col, value):
        self.sudoku[row][col] = value
        return


class Solver():
    def __init__(self, sudoku: Sudoku):
        self.sudoku = sudoku

    # Checks if the susdoku is solved.
    #
    # True if solved, false otherwise.
    def is_solved(self):
        return self.sudoku.consistent() and len(
                                                self.sudoku.
                                                get_open_cells()) == 0

    # Checks if the values is the sudoku are valid.
    #
    # Output: True if invalid, false otherwise.
    def value_error(self):
        for row in range(self.sudoku.size):
            for col in range(self.sudoku.size):
                if self.sudoku.sudoku[row][col] > self.sudoku.size:
                    return True
        return False

    def solver(self, total_sols):
        sols = []
        stack = []

        stack.append(self.sudoku.get_open_cells()[0])

        while stack:
            if  self.sudoku.consistent() is False:
                return None
            if self.is_solved():
                sols.append(self.sudoku.sudoku)
            if len(sols) == total_sols:
                return sols

            # open = self.sudoku.get_open_cells()[0]
            cons = stack.pop()
            val = cons[2].pop()
            self.sudoku.set_cell(cons[0], cons[1], val)
            stack.append(cons)




    def solve(self, total_sols):
        sols = []
        stack = []

        # Checks if the sdoku already violates the rules and is thus not
        # solvable.
        if self.sudoku.consistent() is False:
            return None

        # Checks if the sudoku is already solved. If this is the case, put the
        # current (solved) board in the solutions list.
        if self.is_solved():
            sols.append(copy.deepcopy(self.sudoku.sudoku))
            return sols

        # Checks if any illegal numbers are in the sudoku, returns a ValueError
        # if this is the case.
        if self.value_error() is True:
            raise ValueError

        while len(sols) < total_sols:
            # Sets the current cell to be evaluated to the first element of the
            # list with open cells.
            cells_open = self.sudoku.get_open_cells()
            # If there are no more empty cells, break out of the while loop.
            if len(cells_open) == 0:
                break
            col = cells_open[0][1]
            row = cells_open[0][0]

            # If values are possible for the current cell, place it on the
            # stack.
            if cells_open[0][2]:
                val = cells_open[0][2].pop()
                stack.append([row, col, cells_open[0][2]])
                self.sudoku.set_cell(row, col, val)

                # Checks if the sudoku is now solved. If this is the case and
                # the solution is not yet in the known solutions, add it to the
                # list with known solutions.
                if self.is_solved() and self.sudoku.sudoku not in sols:
                    sols = sols + [(copy.deepcopy(self.sudoku.sudoku))]
                else:
                    continue

            # Checks the stack and tries the values from the constraints on the
            # stack.
            while stack:
                to_check = stack.pop()
                values = to_check[2]
                col = to_check[1]
                row = to_check[0]

                if values:
                    val = values.pop()
                    self.sudoku.set_cell(row, col, val)
                    stack.append([row, col, values])
                    break
                else:
                    self.sudoku.set_cell(row, col, 0)

        # If there are any solutions found, return them. If not, return None.
        if sols:
            return sols
        else:
            return None


# Prints a sudoku (a 2d list). Every row is seperated by a new line and the
# members are space seperated.
#
# Input: A 2d list.
# Output: A list of the values of the row of the matrix.
def print_sudoku(sudoku):
    for row in sudoku:
        print(*row)


if __name__ == "__main__":
    try:
        sud = Sudoku(sys.argv[1])
        solver = Solver(sud)
        if len(sys.argv) == 3:
            total_solutions = int(sys.argv[2])
        else:
            total_solutions = 1

        solutions = solver.solve(total_solutions)
        if solutions:
            for solution in solutions:
                print_sudoku(solution)
        else:
            print("No solution possible")

    except ValueError:
        print("Sudoku invalid: one or more values is incorrect.")
    except FileNotFoundError:
        print("The given file path is incorrect or does not exist.")
    except IndexError:
        print("Your sudoku or input is not the right format.")
    except Exception:
        print("Something went wrong.")
