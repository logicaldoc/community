package com.logicaldoc.core.document;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>DocumentLinkDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
@Repository("documentLinkDAO")
@Transactional
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

        StringBuilder query = new StringBuilder("(_entity.document1.id = :docId or _entity.document2.id = :docId)");
        if (StringUtils.isNotEmpty(type)) {
            query.append(" and _entity.type = :type");
            params.put("type", type);
        }

        return findByWhere(query.toString(), params, null, null);
    }

    @Override
    public DocumentLink findByDocIdsAndType(long docId1, long docId2, String type) throws PersistenceException {
        if (type == null)
            return null;
        return findByWhere("_entity.document1.id = :docId1 and _entity.document2.id = :docId2 and _entity.type = :type",
                Map.of("docId1", docId1, "docId2", docId2, "type", type), null, null).stream().findFirst().orElse(null);
    }

    @Override
    public void delete(long id, int code) throws PersistenceException {
        if (!checkStoringAspect())
            return;

        DocumentLink link = findById(id);
        if (link != null) {
            long docId1 = link.getDocument1() != null ? link.getDocument1().getId() : 0;
            long docId2 = link.getDocument2() != null ? link.getDocument2().getId() : 0;

            super.delete(id, code);
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
        jdbcUpdate("update ld_document set ld_links = (select count(*) from ld_link where ld_deleted = 0 and (ld_docid1 = %d or ld_docid2 = %d)) where ld_id = %d".formatted(docId,docId,docId));
    }
}