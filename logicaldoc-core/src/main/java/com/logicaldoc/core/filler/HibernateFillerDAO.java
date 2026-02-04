package com.logicaldoc.core.filler;

import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of {@link FillerDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
@Repository("fillerDAO")
@Transactional
public class HibernateFillerDAO extends HibernatePersistentObjectDAO<Filler> implements FillerDAO {

	protected HibernateFillerDAO() {
		super(Filler.class);
		super.log = LoggerFactory.getLogger(HibernateFillerDAO.class);
	}

	@Override
	public Filler findByName(String name, long tenantId) throws PersistenceException {
		return findByWhere("_entity.name = :name and _entity.tenantId = :tenantId",
				Map.of("name", name, "tenantId", tenantId), null, null).stream().findFirst().orElse(null);
	}

	@Override
	public void initialize(Filler entity) throws PersistenceException {
		super.initialize(entity);

		if (entity instanceof ChainFiller chained && chained.getChain() != null)
			log.trace("Initialized {} fillers", chained.getChain().size());
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		Filler sampler = findById(id);
		if (sampler != null) {
			sampler.setDeleted(code);
			sampler.setName("%s.%d".formatted(sampler.getName(), sampler.getId()));
			saveOrUpdate(sampler);

			// Removes references to this deleted sampler
			jdbcUpdate("delete from ld_filler_chain where ld_chainedid=" + id);
		}
	}

	@Override
	public <S extends Filler> List<S> findByType(Class<S> type, long tenantId) throws PersistenceException {
		return prepareQuery(
				"from %s _entity where _entity.deleted=0 and _entity.tenantId = :tenantId order by _entity.name asc"
						.formatted(type.getCanonicalName()),
				Map.of("tenantId", tenantId), type, null).list();
	}
}