from itertools import product

from assertpy import assert_that
from behave import *

from src.board import Board
from src.next_generation import NextGeneration

use_step_matcher("re")


@given('the following setup')
def step_impl(context):
    context.board = __get_board(context)


@when('I evolve the board')
def step_impl(context):
    board = context.board
    context.board = NextGeneration.calculate(board)


@then('the center cell should be dead')
def step_impl(context):
    assert_that(context.board.is_alive_at((1, 1))) \
        .described_as('checking if the cell is dead') \
        .is_false()


@then("the center cell should be alive")
def step_impl(context):
    assert_that(context.board.is_alive_at((1, 1))) \
        .described_as('checking if the cell is alive') \
        .is_true()


@then("I should see the following board")
def step_impl(context):
    expected_board = __get_board(context)
    assert_that(context.board.to_cell_array())\
        .is_equal_to(expected_board.to_cell_array())


def __get_board(context):
    board = __headings_and_content(context)
    return Board(board)


def __headings_and_content(context):
    board = [context.table.headings]
    board.extend(r.cells for r in context.table.rows)
    return board
