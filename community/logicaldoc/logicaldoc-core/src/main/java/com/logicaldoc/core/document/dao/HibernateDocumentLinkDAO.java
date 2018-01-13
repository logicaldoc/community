package com.logicaldoc.core.document.dao;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.document.DocumentLink;

/**
 * Hibernate implementation of <code>DocumentLinkDAO</code>
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 4.0
 */
@SuppressWarnings("unchecked")
public class HibernateDocumentLinkDAO extends HibernatePersistentObjectDAO<DocumentLink> implements DocumentLinkDAO {
	public HibernateDocumentLinkDAO() {
		super(DocumentLink.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentLinkDAO.class);
	}

	/**
	 * @see com.logicaldoc.core.document.dao.DocumentLinkDAO#findByDocId(long)
	 */
	@Override
	public List<DocumentLink> findByDocId(long docId) {
		return findByDocId(docId, null);
	}

	/**
	 * @see com.logicaldoc.core.document.dao.DocumentLinkDAO#findByDocId(long,
	 *      java.lang.String)
	 */
	@Override
	public List<DocumentLink> findByDocId(long docId, String type) {
		StringBuffer query = new StringBuffer("(_entity.document1.id = ?1 ");
		query.append("or _entity.document2.id = ?2) ");
		if (StringUtils.isNotEmpty(type)) {
			query.append("and _entity.type = '");
			query.append(type);
			query.append("'");
		}
		return findByWhere(query.toString(), new Object[] { docId, docId }, null, null);
	}

	@Override
	public DocumentLink findByDocIdsAndType(long docId1, long docId2, String type) {
		if (type == null)
			return null;
		DocumentLink link = null;
		StringBuffer query = new StringBuffer("_entity.document1.id = ?1 and _entity.document2.id = ?2 ");
		query.append("and _entity.type = '");
		query.append(type);
		query.append("'");

		List<DocumentLink> links = findByWhere(query.toString(), new Object[] { docId1, docId2 }, null, null);
		if (!links.isEmpty())
			link = links.iterator().next();
		return link;
	}

	@Override
	public boolean delete(long id, int code) {
		DocumentLink link = findById(id);
		if (link != null)
			getCurrentSession().delete(link);
		return true;
	}
}