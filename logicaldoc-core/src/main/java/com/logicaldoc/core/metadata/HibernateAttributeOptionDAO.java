package com.logicaldoc.core.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
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

	public HibernateAttributeOptionDAO() {
		super(AttributeOption.class);
		super.log = LoggerFactory.getLogger(HibernatePersistentObjectDAO.class);
	}

	@Override
	public boolean deleteBySetIdAndAttribute(long setId, String attribute) {
		boolean result = true;
		try {
			List<AttributeOption> options = findByAttribute(setId, attribute);
			for (AttributeOption option : options)
				del(option, PersistentObject.DELETED_CODE_DEFAULT);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public List<AttributeOption> findByAttribute(long setId, String attribute) {
		return findByAttributeAndCategory(setId, attribute, null);
	}

	@Override
	public List<AttributeOption> findByAttributeAndCategory(long setId, String attribute, String category) {
		List<AttributeOption> coll = new ArrayList<AttributeOption>();
		try {
			if (StringUtils.isEmpty(attribute)) {
				if (StringUtils.isEmpty(category))
					coll = (List<AttributeOption>) findByQuery(
							"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = ?1 order by _opt.position asc",
							new Object[] { Long.valueOf(setId) }, null);
				else
					coll = (List<AttributeOption>) findByQuery(
							"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = ?1 and _opt.category = ?2 order by _opt.position asc",
							new Object[] { Long.valueOf(setId), category }, null);
			} else {
				if (StringUtils.isEmpty(category))
					coll = (List<AttributeOption>) findByQuery(
							"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = ?1 and _opt.attribute = ?2 order by _opt.position asc",
							new Object[] { Long.valueOf(setId), attribute }, null);
				else
					coll = (List<AttributeOption>) findByQuery(
							"from AttributeOption _opt where _opt.deleted=0 and _opt.setId = ?1 and _opt.attribute = ?2 and _opt.category = ?3 order by _opt.position asc",
							new Object[] { Long.valueOf(setId), attribute, category }, null);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return coll;
	}

	@Override
	public Map<String, List<AttributeOption>> findByAttributeAsMap(long setId, String attribute) {
		List<AttributeOption> coll = findByAttribute(setId, attribute);
		return coll.stream().collect(Collectors.groupingBy(AttributeOption::getCategory));
	}

	@Override
	public boolean delete(long id, int code) {
		if (!checkStoringAspect())
			return false;

		try {
			AttributeOption option = findById(id);
			del(option, code);
			return true;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	private void del(AttributeOption option, int code) {
		if (option != null) {
			option.setDeleted(code);
			option.setValue(option.getValue() + "." + option.getId());
		}
	}

	@Override
	public void deleteOrphaned(long setId, Collection<String> currentAttributes) {
		try {
			if (currentAttributes == null || currentAttributes.isEmpty() || !checkStoringAspect())
				return;
			StringBuffer buf = new StringBuffer();
			for (String name : currentAttributes) {
				if (buf.length() == 0)
					buf.append("('");
				else
					buf.append(",'");
				buf.append(SqlUtil.doubleQuotes(name));
				buf.append("'");
			}
			buf.append(")");

			List<AttributeOption> options = findByQuery(
					"from AttributeOption _opt where _opt.setId = ?1 and _opt.attribute not in " + buf.toString(),
					new Object[] { setId }, null);

			for (AttributeOption option : options)
				del(option, PersistentObject.DELETED_CODE_DEFAULT);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}
}
