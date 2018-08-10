package com.logicaldoc.core.document.dao;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * Hibernate implementation of <code>DocumentNoteDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
@SuppressWarnings("unchecked")
public class HibernateDocumentNoteDAO extends HibernatePersistentObjectDAO<DocumentNote> implements DocumentNoteDAO {

	public HibernateDocumentNoteDAO() {
		super(DocumentNote.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentNoteDAO.class);
	}

	@Override
	public boolean store(DocumentNote note, History transaction) {
		boolean result = super.store(note);
		if (!result)
			return false;

		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = documentDao.findById(note.getDocId());
		if (doc != null && doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
			documentDao.initialize(doc);
			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDao.store(doc);
		}

		try {
			if (transaction != null) {
				transaction.setEvent(DocumentEvent.NEW_NOTE.toString());
				documentDao.saveDocumentHistory(doc, transaction);
			}
		} catch (Throwable e) {
			if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public List<DocumentNote> findByDocId(long docId) {
		return findByWhere("_entity.docId =" + docId, null, null);
	}

	@Override
	public List<DocumentNote> findByUserId(long userId) {
		return findByWhere("_entity.userId =" + userId, "order by _entity.date desc", null);
	}

	@Override
	public void deleteContentAnnotations(long docId) {
		List<DocumentNote> notes = findByDocId(docId);
		for (DocumentNote note : notes)
			if (note.getPage() != 0)
				delete(note.getId());
		markToIndex(docId);
	}

	private void markToIndex(long docId) {
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = documentDao.findById(docId);
		if (doc != null && doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
			documentDao.initialize(doc);
			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDao.store(doc);
		}
	}

	@Override
	public boolean delete(long id, int code) {
		DocumentNote note=findById(id);
		if(note!=null)
			markToIndex(note.getDocId());
		return super.delete(id, code);
	}
}