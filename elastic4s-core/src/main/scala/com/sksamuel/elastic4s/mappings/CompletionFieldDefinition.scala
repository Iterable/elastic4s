package com.sksamuel.elastic4s.mappings

import com.sksamuel.exts.OptionImplicits._

case class CompletionFieldDefinition(name: String,
                                     analyzer: Option[String] = None,
                                     boost: Option[Double] = None,
                                     coerce: Option[Boolean] = None,
                                     copyTo: Seq[String] = Nil,
                                     docValues: Option[Boolean] = None,
                                     enabled: Option[Boolean] = None,
                                     fields: Seq[FieldDefinition] = Nil,
                                     fielddataFrequencyFilter: Option[FielddataFrequencyFilter] = None,
                                     includeInAll: Option[Boolean] = None,
                                     index: Option[String] = None,
                                     indexOptions: Option[String] = None,
                                     maxInputLength: Option[Int] = None,
                                     norms: Option[Boolean] = None,
                                     normalizer: Option[String] = None,
                                     nullable: Option[Boolean] = None,
                                     nullValue: Option[Any] = None,
                                     preserveSeparators: Option[Boolean] = None,
                                     preservePositionIncrements: Option[Boolean] = None,
                                     searchAnalyzer: Option[String] = None,
                                     store: Option[Boolean] = None,
                                     termVector: Option[String] = None
                                    ) extends FieldDefinition {

  type T = CompletionFieldDefinition
  override def `type` = "completion"

  override def analyzer(analyzer: String): T = copy(analyzer = analyzer.some)
  override def boost(boost: Double): T = copy(boost = boost.some)
  override def docValues(docValues: Boolean): T = copy(docValues = docValues.some)

  override def fields(fields: Iterable[FieldDefinition]): T = copy(fields = fields.toSeq)

  def coerce(coerce: Boolean): T = copy(coerce = coerce.some)
  override def copyTo(first: String, rest: String*): T = copyTo(first +: rest)
  override def copyTo(copyTo: Iterable[String]): T = copy(copyTo = copyTo.toSeq)

  override def enabled(enabled: Boolean): T = copy(enabled = enabled.some)

  override def includeInAll(includeInAll: Boolean): T = copy(includeInAll = includeInAll.some)
  override def index(index: Boolean): T = copy(index = index.toString.some)

  override def norms(norms: Boolean): T = copy(norms = norms.some)
  override def normalizer(normalizer: String): T = copy(normalizer = normalizer.some)
  override def nullable(nullable: Boolean): T = copy(nullable = nullable.some)
  override def nullValue(nullvalue: Any): T = copy(nullValue = nullvalue.some)

  override def store(b: Boolean): T = copy(store = b.some)
  override def searchAnalyzer(analyzer: String): T = copy(searchAnalyzer = analyzer.some)

  override def termVector(t: String): T = copy(termVector = t.some)

  def preserveSeparators(preserve: Boolean): T = copy(preserveSeparators = preserve.some)
  def preservePositionIncrements(preserve: Boolean): T = copy(preservePositionIncrements = preserve.some)
  def maxInputLength(maxInputLength: Int): T = copy(maxInputLength = maxInputLength.some)
}
