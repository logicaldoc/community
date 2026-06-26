package com.logicaldoc.core.document;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.concurrent.FutureElaboration;

/**
 * A future used to track asynchronous elaborations on a document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class DocumentFuture extends FutureElaboration<Document, Document> {

    private static final Logger log = LoggerFactory.getLogger(DocumentFuture.class);

    public DocumentFuture(Document document, Future<Document> future) {
        super(document, future);
    }

    public Document getDocument() throws PersistenceException {
        Document doc = null;
        try {
            doc = super.get();
        } catch (InterruptedException e) {
            log.warn("Interrupted! Current thread gets killed.", e);
            Thread.currentThread().interrupt();
        } catch (ExecutionException e) {
            throw new PersistenceException("Value cannot be retrieved", e);
        }

        if (doc == null)
            throw new PersistenceException("Unknown database error");

        return doc;
    }
}