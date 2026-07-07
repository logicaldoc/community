package com.logicaldoc.core.dashlet;

import java.util.Map;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>DashletDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
@Repository("dashletDAO")
@Transactional
public class HibernateDashletDAO extends HibernatePersistentObjectDAO<Dashlet> implements DashletDAO {

    protected HibernateDashletDAO() {
        super(Dashlet.class);
        super.log = LoggerFactory.getLogger(HibernateDashletDAO.class);
    }

    @Override
    public Dashlet findByName(String name, long tenantId) throws PersistenceException {
        return findByWhere("_entity.tenantId = :tenantId and _entity.name = :name",
                Map.of("tenantId", tenantId, "name", name), null, null).stream().findFirst().orElse(null);
    }

    @Override
    public void delete(long id, int code) throws PersistenceException {
        if (!checkStoringAspect())
            return;

        Dashlet dashlet = findById(id);
        dashlet.setName("%s.%d".formatted(dashlet.getName(), id));
        dashlet.setDeleted(code);
        saveOrUpdate(dashlet);
    }
}