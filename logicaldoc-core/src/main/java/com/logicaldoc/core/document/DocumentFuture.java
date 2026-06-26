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

    private final static Logger log = LoggerFactory.getLogger(DocumentFuture.class);

    public DocumentFuture(Document document, Future<Document> future) {
        super(document, future);
    }

    public Document getDocument() throws PersistenceException {
        Document doc = null;
        try {
            doc = super.get();
        } catch (InterruptedException | ExecutionException e) {
            // Do nothing
            log.warn(e.getMessage(), e);
        }
        
        if (doc == null)
            throw new PersistenceException("Unknown database error");
        
        return doc;
    }
}