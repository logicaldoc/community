package com.logicaldoc.core.document;

import java.io.IOException;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.task.AbstractDocumentProcessor;

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
	protected String getDefaultUser() {
		return "_system";
	}

	@Override
	protected int getBatchSize() {
		return config.getInt("digest.batch", 500);
	}

	@Override
	protected void processDocument(Document document, User user) throws PersistenceException, IOException {
		documentDao.updateDigest(document);
	}

	@Override
	protected void prepareQueueQuery(StringBuilder where, StringBuilder sort) {
		where.append(PersistentObjectDAO.ENTITY);
		where.append(".digest is null and ");
		where.append(PersistentObjectDAO.ENTITY);
		where.append(".docRef is null");
	}
}