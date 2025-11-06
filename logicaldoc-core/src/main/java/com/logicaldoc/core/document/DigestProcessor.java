package com.logicaldoc.core.document;

import java.io.IOException;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.task.AbstractDocumentProcessor;
import com.logicaldoc.core.task.DocumentProcessorCallable;
import com.logicaldoc.core.task.DocumentProcessorStats;

/**
 * This task takes care of calculating the documents digest
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@Component("digestProcessor")
public class DigestProcessor extends AbstractDocumentProcessor {

	public static final String NAME = "DigestProcessor";

	public DigestProcessor() {
		super(NAME);
		log = LoggerFactory.getLogger(DigestProcessor.class);
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	@Override
	protected int getBatchSize() {
		return config.getInt("digest.batch", 500);
	}

	@Override
	protected void prepareQueueQuery(StringBuilder where, StringBuilder sort) {
		where.append(PersistentObjectDAO.ENTITY);
		where.append(".digest is null and ");
		where.append(PersistentObjectDAO.ENTITY);
		where.append(".docRef is null");
	}

	@Override
	protected DocumentProcessorCallable<DocumentProcessorStats> prepareCallable(List<Long> docIds) {
		return new DocumentProcessorCallable<DocumentProcessorStats>(docIds, this, log) {

			@Override
			protected void processDocument(Document document, User user) throws PersistenceException, IOException {
				DocumentDAO.get().updateDigest(document);
				if (log.isDebugEnabled())
					log.debug("Digested document {}: {}", document, StringUtils.abbreviate(document.getDigest(), 100));
			}

			@Override
			protected DocumentProcessorStats prepareStats() {
				return new DocumentProcessorStats();
			}
		};
	}
}