package com.logicaldoc.core.metadata;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.history.HibernatePersistentObjectDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>AttributeOptionDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
@SuppressWarnings("unchecked")
public class HibernateAttributeOptionDAO extends HibernatePersistentObjectDAO<AttributeOption>
		implements AttributeOptionDAO {

	private static final String SET_ID = "setId";

	public HibernateAttributeOptionDAO() {
		super(AttributeOption.class);
		super.log = LoggerFactory.getLogger(HibernatePersistentObjectDAO.class);
	}

	@Override
	public void deleteBySetIdAndAttribute(long setId, String attribute) throws PersistenceException {
		List<AttributeOption> options = findByAttribute(setId, attribute);
		for (AttributeOption option : options)
			del(option, PersistentObject.DELETED_CODE_DEFAULT);
	}

	@Override
	public List<AttributeOption> findByAttribute(long setId, String attribute) throws PersistenceException {
		return findByAttributeAndCategory(setId, attribute, null);
	}

	@Override
	public List<AttributeOption> findByAttributeAndCategory(long setId, String attribute, String category)
			throws PersistenceException {
		List<AttributeOption> coll;

		if (StringUtils.isEmpty(attribute)) {
			if (StringUtils.isEmpty(category)) {
				Map<String, Object> params = new HashMap<>();
				params.put(SET_ID, Long.valueOf(setId));

				coll = findByQuery(
						"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = :setId order by _opt.position asc",
						params, null);
			} else {
				Map<String, Object> params = new HashMap<>();
				params.put(SET_ID, Long.valueOf(setId));
				params.put("category", category);

				coll = findByQuery(
						"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = :setId and _opt.category = :category order by _opt.position asc",
						params, null);
			}
		} else {
			if (StringUtils.isEmpty(category)) {
				Map<String, Object> params = new HashMap<>();
				params.put(SET_ID, Long.valueOf(setId));
				params.put("attribute", attribute);

				coll = findByQuery(
						"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = :setId and _opt.attribute = :attribute order by _opt.position asc",
						params, null);
			} else {
				Map<String, Object> params = new HashMap<>();
				params.put(SET_ID, Long.valueOf(setId));
				params.put("category", category);
				params.put("attribute", attribute);

				coll = findByQuery(
						"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = :setId and _opt.attribute = :attribute and _opt.category = :category order by _opt.position asc",
						params, null);
			}
		}

		return coll;
	}

	@Override
	public Map<String, List<AttributeOption>> findByAttributeAsMap(long setId, String attribute)
			throws PersistenceException {
		List<AttributeOption> coll = findByAttribute(setId, attribute);
		return coll.stream().collect(Collectors.groupingBy(AttributeOption::getCategory));
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		AttributeOption option = findById(id);
		del(option, code);
	}

	private void del(AttributeOption option, int code) {
		if (option != null) {
			option.setDeleted(code);
			option.setValue(option.getValue() + "." + option.getId());
		}
	}

	@Override
	public void deleteOrphaned(long setId, Collection<String> currentAttributes) throws PersistenceException {
		if (currentAttributes == null || currentAttributes.isEmpty() || !checkStoringAspect())
			return;
		StringBuilder buf = new StringBuilder();
		for (String name : currentAttributes) {
			if (buf.length() == 0)
				buf.append("('");
			else
				buf.append(",'");
			buf.append(SqlUtil.doubleQuotes(name));
			buf.append("'");
		}
		buf.append(")");

		Map<String, Object> params = new HashMap<>();
		params.put(SET_ID, setId);

		List<AttributeOption> options = findByQuery(
				"from AttributeOption _opt where _opt.setId = :setId and _opt.attribute not in " + buf.toString(),
				params, null);

		for (AttributeOption option : options)
			del(option, PersistentObject.DELETED_CODE_DEFAULT);
	}
}
