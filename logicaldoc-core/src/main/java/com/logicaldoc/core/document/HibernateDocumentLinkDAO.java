package com.logicaldoc.core.document;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernatePersistentObjectDAO;

/**
 * Hibernate implementation of <code>DocumentLinkDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
public class HibernateDocumentLinkDAO extends HibernatePersistentObjectDAO<DocumentLink> implements DocumentLinkDAO {
	public HibernateDocumentLinkDAO() {
		super(DocumentLink.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentLinkDAO.class);
	}

	@Override
	public List<DocumentLink> findByDocId(long docId) throws PersistenceException {
		return findByDocId(docId, null);
	}

	@Override
	public List<DocumentLink> findByDocId(long docId, String type) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("docId", docId);

		StringBuilder query = new StringBuilder("(" + ENTITY + ".document1.id = :docId ");
		query.append(" or " + ENTITY + ".document2.id = :docId) ");
		if (StringUtils.isNotEmpty(type)) {
			query.append("and " + ENTITY + ".type = :type");
			params.put("type", type);
		}

		return findByWhere(query.toString(), params, null, null);
	}

	@Override
	public DocumentLink findByDocIdsAndType(long docId1, long docId2, String type) throws PersistenceException {
		if (type == null)
			return null;

		DocumentLink link = null;
		StringBuilder query = new StringBuilder(
				ENTITY + ".document1.id = :docId1 and " + ENTITY + ".document2.id = :docId2 ");
		query.append(" and " + ENTITY + ".type = :type");

		Map<String, Object> params = new HashMap<>();
		params.put("docId1", docId1);
		params.put("docId2", docId2);
		params.put("type", type);

		List<DocumentLink> links = findByWhere(query.toString(), params, null, null);

		if (!links.isEmpty())
			link = links.iterator().next();
		return link;
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		DocumentLink link = findById(id);
		if (link != null) {
			long docId1 = link.getDocument1() != null ? link.getDocument1().getId() : 0;
			long docId2 = link.getDocument2() != null ? link.getDocument2().getId() : 0;

			getCurrentSession().delete(link);

			flush();

			updateLinksCount(docId1);
			updateLinksCount(docId2);
		}

	}

	@Override
	public void store(DocumentLink entity) throws PersistenceException {
		boolean newLink = entity.getId() == 0L;
		super.store(entity);

		flush();

		if (newLink) {
			if (entity.getDocument1() != null)
				updateLinksCount(entity.getDocument1().getId());
			if (entity.getDocument2() != null)
				updateLinksCount(entity.getDocument2().getId());
		}
	}

	private void updateLinksCount(long docId) throws PersistenceException {
		jdbcUpdate("update ld_document set ld_links = (select count(*) from ld_link  where ld_deleted=0 and (ld_docid1="
				+ docId + " or ld_docid2=" + docId + ")) where ld_id=" + docId);
	}
}