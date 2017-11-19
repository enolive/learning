import unittest

from assertpy import assert_that

from src.board import Board


class BoardTests(unittest.TestCase):
    def test_that_cell_is_dead_by_default(self):
        board = Board([])
        assert_that(board.is_alive_at((0, 0))).is_false()

    def test_that_already_dead_cell_can_be_set_dead_again(self):
        board = Board(['.'])
        board.set_dead_at((0, 0))
        assert_that(board.is_alive_at((0, 0))).is_false()

    def test_that_cell_that_was_set_alive_is_alive(self):
        board = Board([['.', '.', '.'], ['.', '.', '.'], ['.', '.', '.']])
        board.set_alive_at((0, 1))
        assert_that(board.is_alive_at((0, 1))).is_true()

    def test_that_living_cell_can_be_killed(self):
        board = Board([['.', '.', '.'], ['x', '.', '.'], ['.', '.', '.']])
        board.set_dead_at((0, 1))
        assert_that(board.is_alive_at((0, 1))).is_false()

    def test_that_cell_has_three_living_neighbours(self):
        board = Board([['x', 'x', 'x'], ['.', '.', '.'], ['.', '.', '.']])
        assert_that(board.count_living_neighbours_of((1, 1))).is_equal_to(3)

    def test_that_cell_has_two_living_neighbours(self):
        board = Board([['x', 'x', '.'], ['.', '.', '.'], ['.', '.', '.']])
        assert_that(board.count_living_neighbours_of((1, 1))).is_equal_to(2)

    def test_that_living_cell_has_two_living_neighbours(self):
        board = Board([['x', 'x', '.'], ['.', 'x', '.'], ['.', '.', '.']])
        assert_that(board.count_living_neighbours_of((1, 1))).is_equal_to(2)

    def test_that_cell_on_border_has_two_living_neighbours(self):
        board = Board([['x', 'x', '.'], ['x', '.', '.'], ['.', '.', '.']])
        assert_that(board.count_living_neighbours_of((0, 0))).is_equal_to(2)

    def test_that_board_is_dumped_correctly(self):
        assert_that(Board([['x', 'x', '.'], ['.', 'x', 'x']]).to_cell_array())\
             .is_equal_to([['x', 'x', '.'], ['.', 'x', 'x']])

    def test_that_board_has_the_expected_dimensions(self):
        assert_that(Board([['x', 'x', '.'], ['.', 'x', 'x']]).dimensions()).is_equal_to((3, 2))
        assert_that(Board([['x', 'x', '.'], ['.', 'x', '.']]).dimensions()).is_equal_to((2, 2))

    def test_that_empty_board_has_zero_dimensions(self):
        board = Board([['.', '.'], ['.', '.']])
        assert_that(board.dimensions()).is_equal_to((0, 0))

    def test_that_something(self):
        assert_that([(2, 3), (2, 1), (2, 2)]).contains_only((2, 3), (2, 1), (2, 2))
