package com.sksamuel.elastic4s.http.search

import com.sksamuel.elastic4s.http.{EnumConversions, ScriptBuilderFn}
import com.sksamuel.elastic4s.http.search.aggs.AggregationBuilderFn
import com.sksamuel.elastic4s.http.search.collapse.CollapseBuilderFn
import com.sksamuel.elastic4s.http.search.queries.{QueryBuilderFn, SortContentBuilder}
import com.sksamuel.elastic4s.http.search.suggs.TermSuggestionBuilderFn
import com.sksamuel.elastic4s.json.{XContentBuilder, XContentFactory}
import com.sksamuel.elastic4s.searches.SearchDefinition
import com.sksamuel.elastic4s.searches.suggestion.TermSuggestionDefinition

object SearchBodyBuilderFn {

  def apply(request: SearchDefinition): XContentBuilder = {

    val builder = XContentFactory.jsonBuilder()

    request.query.map(QueryBuilderFn.apply).foreach(x => builder.rawField("query", x.string))
    request.postFilter.map(QueryBuilderFn.apply).foreach(x => builder.rawField("post_filter", x.string))

    request.from.foreach(builder.field("from", _))
    request.size.foreach(builder.field("size", _))

    request.minScore.foreach(builder.field("min_score", _))
    if (request.searchAfter.nonEmpty) {
      builder.autoarray("search_after", request.searchAfter)
    }

    if (request.scriptFields.nonEmpty) {
      builder.startObject("script_fields")
      request.scriptFields.foreach { field =>
        builder.startObject(field.field)
        builder.rawField("script", ScriptBuilderFn(field.script))
        builder.endObject()
      }
      builder.endObject()
    }

    if (request.sorts.nonEmpty) {
			builder.startArray("sort")
			// Workaround for bug where separator is not added with rawValues
      val arrayBody = request.sorts.map(s => SortContentBuilder(s).string).mkString(",")
      builder.rawValue(arrayBody)
			builder.endArray()
    }

    if (request.storedFields.nonEmpty) {
      builder.array("stored_fields", request.storedFields.toArray)
    }

    // source filtering
    request.fetchContext foreach { context =>
      if (context.fetchSource) {
        if (context.includes.nonEmpty || context.excludes.nonEmpty) {
          builder.startObject("_source")
          builder.array("includes", context.includes)
          builder.array("excludes", context.excludes)
          builder.endObject()
        }
      } else {
        builder.field("_source", false)
      }
    }

    if (request.docValues.nonEmpty)
      builder.array("docvalue_fields", request.docValues.toArray)

    // aggregations
    if (request.aggs.nonEmpty) {
      builder.startObject("aggs")
      request.aggs.foreach { agg =>
        builder.rawField(agg.name, AggregationBuilderFn(agg))
      }
      builder.endObject()
    }

    builder.endObject()
  }
}
