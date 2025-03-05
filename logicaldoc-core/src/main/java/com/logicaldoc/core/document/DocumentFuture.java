package com.logicaldoc.core.document;

import java.util.concurrent.Future;

import com.logicaldoc.util.concurrent.FutureElaboration;

/**
 * A future used to track asynchronous elaborations on a document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class DocumentFuture extends FutureElaboration<Document, Document> {

	public DocumentFuture(Document document, Future<Document> future) {
		super(document, future);
	}

	public Document getDocument() {
		return super.getObject();
	}
}