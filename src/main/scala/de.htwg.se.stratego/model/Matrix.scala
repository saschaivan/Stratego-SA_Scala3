package de.htwg.se.stratego.model

case class Matrix[Field](rows: Vector[Vector[Field]]) {

  def this(rowSize: Int, colSize: Int, field: Field) = this(Vector.tabulate(rowSize, colSize) { (row, col) => field })

  def field(row: Int, col: Int): Field = rows(row)(col)

  def matrixSize: Int = rows.size

  def updateField(row: Int, col: Int, field: Field): Matrix[Field] = copy(rows.updated(row, rows(row).updated(col, field)))

}
