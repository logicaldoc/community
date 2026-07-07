package com.logicaldoc.core.generic;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>GenericDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Repository("genericDAO")
@Transactional
public class HibernateGenericDAO extends HibernatePersistentObjectDAO<Generic> implements GenericDAO {

    public HibernateGenericDAO() {
        super(Generic.class);
        super.log = LoggerFactory.getLogger(HibernateGenericDAO.class);
    }

    @Override
    public void delete(long genericId, int code) throws PersistenceException {
        if (code == 0)
            throw new IllegalArgumentException("code cannot be 0");

        if (!checkStoringAspect())
            return;

        Generic generic = findById(genericId);
        if (generic != null) {
            generic.setType("%s.%d".formatted(generic.getType(), generic.getId()));
            generic.setSubtype("%s.%d".formatted(generic.getSubtype(), generic.getId()));
            generic.setDeleted(code);
            saveOrUpdate(generic);
            flush();
        }
    }

    @Override
    public Generic findByAlternateKey(String type, String subtype, Long qualifier, long tenantId)
            throws PersistenceException {

        StringBuilder sb = new StringBuilder(
                "_entity.type = :type and _entity.subtype = :subtype and _entity.tenantId = :tenantId ");
        if (qualifier != null)
            sb.append(" and _entity.qualifier = %d".formatted(qualifier));
        else
            sb.append(" and _entity.qualifier is null");
        List<Generic> coll = findByWhere(sb.toString(), Map.of("type", type, "subtype", subtype, "tenantId", tenantId),
                null, null);
        if (CollectionUtils.isNotEmpty(coll))
            return coll.get(0);
        else
            return null;
    }

    @Override
    public List<Generic> findByTypeAndSubtype(String type, String subtype, Long qualifier, Long tenantId)
            throws PersistenceException {
        Map<String, Object> params = new HashMap<>();
        StringBuilder query = new StringBuilder(" 1=1 ");
        if (StringUtils.isNotEmpty(type)) {
            query.append(" and _entity.type like :type");
            params.put("type", type);
        }
        if (StringUtils.isNotEmpty(subtype)) {
            query.append(" and _entity.subtype like :subtype");
            params.put("subtype", subtype);
        }
        if (qualifier != null) {
            query.append(" and _entity.qualifier = :qualifier");
            params.put("qualifier", qualifier);
        }
        if (tenantId != null) {
            query.append(" and _entity.tenantId = :tenantId");
            params.put("tenantId", tenantId);
        }

        return findByWhere(query.toString(), params, null, null);
    }

    @Override
    public void initialize(Generic generic) {
        refresh(generic);
        for (String attribute : generic.getAttributes().keySet()) {
            if (generic.getValue(attribute) != null)
                generic.getValue(attribute).toString();
        }
    }

    @Override
    public void store(Generic entity) throws PersistenceException {
        super.store(entity);
        flush();
    }
}