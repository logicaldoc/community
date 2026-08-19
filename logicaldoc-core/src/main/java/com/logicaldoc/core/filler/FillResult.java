package com.logicaldoc.core.filler;

import com.logicaldoc.core.document.Document;

/**
 * The result of a fill.It provides the modified document and a representation
 * of the filled content.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.1
 */
public record FillResult(Document document, String content) {

}