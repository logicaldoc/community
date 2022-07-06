package com.logicaldoc.core.document.dao;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * Hibernate implementation of <code>DocumentNoteDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class HibernateDocumentNoteDAO extends HibernatePersistentObjectDAO<DocumentNote> implements DocumentNoteDAO {

	public HibernateDocumentNoteDAO() {
		super(DocumentNote.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentNoteDAO.class);
	}

	@Override
	public boolean store(DocumentNote note, DocumentHistory transaction) throws PersistenceException {
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
	public List<DocumentNote> findByDocId(long docId, String fileVersion) {
		return findByDocIdAndType(docId, fileVersion, null);
	}

	@Override
	public List<DocumentNote> findByDocIdAndType(long docId, String fileVersion, String type) {
		return findByDocIdAndTypes(docId, fileVersion, StringUtils.isEmpty(type) ? null : Arrays.asList(type));
	}

	@Override
	public List<DocumentNote> findByDocIdAndTypes(long docId, String fileVersion, Collection<String> types) {
		try {
			if (StringUtils.isEmpty(fileVersion))
				if (types == null || types.isEmpty())
					return findByWhere("_entity.docId = " + docId, null, null);
				else
					return findByWhere("_entity.docId = ?1 and _entity.type in (?2)", new Object[] { docId, types },
							null, null);
			else if (types == null || types.isEmpty())
				return findByWhere("_entity.docId = ?1 and _entity.fileVersion = ?2",
						new Object[] { docId, fileVersion }, null, null);
			else
				return findByWhere("_entity.docId = ?1 and _entity.fileVersion = ?2 and _entity.type in (?3)",
						new Object[] { docId, fileVersion, types }, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentNote>();
		}
	}

	@Override
	public List<DocumentNote> findByUserId(long userId) {
		try {
			return findByWhere("_entity.userId =" + userId, "order by _entity.date desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentNote>();
		}
	}

	private void markToIndex(long docId) throws PersistenceException {
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = documentDao.findById(docId);
		if (doc != null && doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
			documentDao.initialize(doc);
			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDao.store(doc);
		}
	}

	@Override
	public boolean delete(long id, int code) throws PersistenceException {
		DocumentNote note = findById(id);
		if (note != null)
			markToIndex(note.getDocId());
		return super.delete(id, code);
	}

	@Override
	public int copyAnnotations(long docId, String oldFileVersion, String newFileVersion) throws PersistenceException {
		List<DocumentNote> oldNotes = findByDocId(docId, oldFileVersion);
		int count = 0;
		for (DocumentNote oldNote : oldNotes) {
			if (oldNote.getPage() > 0)
				continue;

			try {
				DocumentNote newNote = (DocumentNote) oldNote.clone();
				newNote.setId(0L);
				newNote.setFileVersion(newFileVersion);
				store(newNote);
				count++;
			} catch (CloneNotSupportedException e) {
				log.error(e.getMessage(), e);
			}
		}
		return count;
	}
}