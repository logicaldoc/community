package com.logicaldoc.core.searchengine.folder;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Search specialization for Folder searches.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FolderSearch extends Search {

	public FolderSearch() {
	}

	@SuppressWarnings("unchecked")
	@Override
	public void internalSearch() throws SearchException {
		UserDAO userDAO = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDAO.findById(options.getUserId());
		userDAO.initialize(user);

		Object[] params = prepareExpression();
		options.setParameters(params);

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		// Execute the search
		List<Hit> folders;
		try {
			folders = (List<Hit>) dao.query(options.getExpression(), params, new HitMapper(), null);
		} catch (PersistenceException e) {
			throw new SearchException(e);
		}
		estimatedHitsNumber = folders.size();

		// Traverse the results checking visibility and count
		Collection<Long> accessibleFolderIds = findAccessibleFolderIds(user);
		for (Hit folder : folders) {
			if (accessibleFolderIds.contains(folder.getId())
					|| (accessibleFolderIds.isEmpty() && user.isMemberOf("admin")))
				hits.add(folder);
			if (hits.size() >= options.getMaxHits())
				break;
		}

		if (hits.size() < options.getMaxHits())
			estimatedHitsNumber = hits.size();
	}

	/**
	 * Utility method that prepares the query expression
	 */
	private Object[] prepareExpression() {
		if (StringUtils.isNotEmpty(options.getExpression()))
			return options.getParameters();

		ArrayList<Object> params = new ArrayList<Object>();

		StringBuffer query = new StringBuffer();

		if (options.isRetrieveAliases())
			query.append("(");

		// Find all real folders
		query.append(
				"select A.ld_id, A.ld_parentid, A.ld_name, A.ld_description, A.ld_creation, A.ld_lastmodified, A.ld_type, A.ld_foldref, C.ld_name, A.ld_templateid ");
		query.append(" from ld_folder A ");
		query.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");

		appendWhereClause(false, params, query);

		if (options.isRetrieveAliases()) {
			// Append all aliases
			query.append(
					") UNION (select A.ld_id, A.ld_parentid, A.ld_name, REF.ld_description, A.ld_creation, A.ld_lastmodified, A.ld_type, A.ld_foldref, C.ld_name, A.ld_templateid ");
			query.append(" from ld_folder A ");
			query.append(" join ld_folder REF on A.ld_foldref=REF.ld_id ");
			query.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");
			appendWhereClause(true, params, query);
			query.append(")");
		}

		options.setExpression(query.toString());

		log.info("executing query {}", query.toString());
		log.info("with parameters {}", params);

		return params.toArray();
	}

	/**
	 * This method appends the where clause considering or not the aliases on
	 * the search.
	 * 
	 * @param searchAliases If true, also the aliases must be considered in the
	 *        search
	 * @param params
	 * @param query
	 */
	private void appendWhereClause(boolean searchAliases, ArrayList<Object> params, StringBuffer query) {
		String tableAlias = "A";
		if (searchAliases)
			tableAlias = "REF";

		/*
		 * Prepare the joins for the extended attributes. If the TOP condition
		 * is AND we have to do multiple joins(one per criteria), otherwise if
		 * the TOP operator is OR we have to use just one join to avoid high
		 * load on the database.
		 */
		int counter = 0;
		if (((FolderSearchOptions) options).getCriteria() != null)
			for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria()) {
				if (criterion.isExtendedAttribute()) {
					if (!criterion.isEmpty()) {
						counter++;
						query.append(", ld_folder_ext C" + counter);
						if ("or".equals(options.getTopOperator()))
							break;
					}
				}
			}

		query.append(" where A.ld_deleted=0 and A.ld_hidden=0 ");
		query.append(" and A.ld_type=" + (searchAliases ? Folder.TYPE_ALIAS : Folder.TYPE_DEFAULT) + "  ");

		long tenantId = Tenant.DEFAULT_ID;
		if (options.getTenantId() != null)
			tenantId = options.getTenantId().longValue();
		else if (searchUser != null)
			tenantId = searchUser.getTenantId();

		query.append(" and A.ld_tenantid=" + tenantId);

		if (options.getTemplate() != null)
			query.append(" and " + tableAlias + ".ld_templateid=" + options.getTemplate());

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		FolderSearchOptions fOptions = (FolderSearchOptions) options;
		if (fOptions.getFolderId() != null) {
			query.append(" and ");
			if (fOptions.isSearchInSubPath()) {
				String path = dao.computePath(fOptions.getFolderId());
				query.append(tableAlias + ".ld_path like '" + path + "/%'");
			} else
				query.append(tableAlias + ".ld_parentid = " + fOptions.getFolderId());
		}

		String composition = null;
		if ("not".equals(options.getTopOperator())) {
			composition = "and not";
		} else {
			composition = options.getTopOperator();
		}

		/*
		 * If a top operator was specified, it will overwrite all criteria
		 * operators
		 */
		if (composition != null && fOptions.getCriteria() != null)
			for (FolderCriterion criterion : fOptions.getCriteria())
				criterion.setComposition(composition);

		if (searchAliases)
			query.append(" and A.ld_foldref is not null and REF.ld_deleted=0 and A.ld_foldref = REF.ld_id ");
		else
			query.append(" and A.ld_foldref is null ");

		// Now add all criteria
		boolean first = true;
		counter = 0;

		if (options != null && fOptions.getCriteria() != null)
			for (FolderCriterion criterion : fOptions.getCriteria()) {
				if (criterion.isEmpty())
					continue;

				if (first) {
					query.append(" and ( ");
					if ("not".equals(options.getTopOperator()))
						query.append("not");
				} else
					query.append(" " + criterion.getComposition());
				first = false;
				query.append("(");

				String columnName = "";
				if (criterion.isExtendedAttribute()) {
					// In case of OR top operator we only have a single join
					// with extended attributes
					if (!"or".equals(options.getTopOperator()) || counter == 0)
						counter++;
					query.append("(C" + counter + ".ld_folderid=" + tableAlias + ".ld_id");
					query.append(" and (C" + counter + ".ld_name='");
					query.append(criterion.getFieldName());
					query.append("' or C" + counter + ".ld_name like '");
					query.append(criterion.getFieldName());
					query.append("-%') and ");
					columnName = "C" + counter + ".";
					switch (criterion.getType()) {
					case Attribute.TYPE_STRING:
						columnName += "ld_stringvalue";
						break;
					case Attribute.TYPE_INT:
					case Attribute.TYPE_USER:
					case Attribute.TYPE_FOLDER:
					case Attribute.TYPE_BOOLEAN:
						columnName += "ld_intvalue";
						break;
					case Attribute.TYPE_DOUBLE:
						columnName += "ld_doublevalue";
						break;
					case Attribute.TYPE_DATE:
						columnName += "ld_datevalue";
						break;
					}
				} else {
					columnName = criterion.getColumnName();

					// If the column name is not qualified, prepend the current
					// table alias
					if (!columnName.contains("."))
						columnName = tableAlias + "." + columnName;
				}

				switch (criterion.getType()) {
				case Attribute.TYPE_STRING:
					if (options.isCaseSensitive()) {
						String val = SqlUtil.doubleQuotes(criterion.getStringValue());
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append(columnName + " = '" + val + "'");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = '" + val + "')");
						else if (FolderCriterion.OPERATOR_CONTAINS.equals(criterion.getOperator()))
							query.append(columnName + " like '%" + val + "%'");
						else if (FolderCriterion.OPERATOR_NOTCONTAINS.equals(criterion.getOperator()))
							query.append(
									" (" + columnName + " is null or not (" + columnName + " like '%" + val + "%'))");
						else if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator()))
							query.append(columnName + " is null ");
						else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator()))
							query.append(columnName + " is not null ");
					} else {
						String val = SqlUtil.doubleQuotes(criterion.getStringValue().toLowerCase());
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append("lower(" + columnName + ") = '" + val + "'");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not lower(" + columnName + ") = '" + val + "')");
						else if (FolderCriterion.OPERATOR_CONTAINS.equals(criterion.getOperator()))
							query.append("lower(" + columnName + ") like '%" + val + "%'");
						else if (FolderCriterion.OPERATOR_NOTCONTAINS.equals(criterion.getOperator()))
							query.append(" (" + columnName + " is null or not (lower(" + columnName + ") like '%" + val
									+ "%')");
						else if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator()))
							query.append(columnName + " is null ");
						break;
					}
					break;
				case Attribute.TYPE_INT:
					if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					} else {
						params.add(criterion.getLongValue());
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append(columnName + " = ?");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = ?)");
						else if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
							query.append(columnName + " > ?");
						else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
							query.append(columnName + " < ?");
					}
					break;
				case Attribute.TYPE_FOLDER:
				case Attribute.TYPE_USER:
					if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					} else {
						params.add(criterion.getLongValue());
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append(columnName + " = ?");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = ?)");
					}
					break;
				case Attribute.TYPE_BOOLEAN:
					if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator())) {
						params.add(criterion.getLongValue());
						query.append(columnName + " = ?");
					} else if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					}
					break;
				case Attribute.TYPE_DOUBLE:
					if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					} else {
						params.add(criterion.getDoubleValue());
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append(columnName + " = ?");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = ?)");
						else if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
							query.append(columnName + " > ?");
						else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
							query.append(columnName + " < ?");
					}
					break;
				case Attribute.TYPE_DATE:
					if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					} else {
						params.add(criterion.getSqlDateValue());
						if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
							query.append(columnName + " > ?");
						else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
							query.append(columnName + " < ?");
					}
					break;
				case FolderCriterion.TYPE_TEMPLATE:
					if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					} else {
						params.add(criterion.getLongValue());
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append(columnName + " = ?");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = ?)");
					}
					break;
				case FolderCriterion.TYPE_LANGUAGE:
					if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
						query.append(columnName + " is null ");
					} else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
						query.append(columnName + " is not null ");
					} else {
						String val2 = criterion.getStringValue();
						if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
							query.append(columnName + " = '" + val2 + "'");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = '" + val2 + "')");
					}
					break;
				case FolderCriterion.TYPE_FOLDER:
					if (FolderCriterion.OPERATOR_INORSUBFOLDERS.equals(criterion.getOperator())) {
						String path = dao.computePath(criterion.getLongValue());
						query.append(tableAlias + ".ld_path like '" + path + "/%'");
					} else
						query.append(columnName + " = " + criterion.getLongValue());
					break;
				}

				if (criterion.isExtendedAttribute())
					query.append(")");
				query.append(")");
			}
		if (!first)
			query.append(")");

		FolderSearchOptions poptions = (FolderSearchOptions) options;
		if (poptions.getOrder() != null && poptions.getOrder().length > 0) {
			query.append(" order by ");
			for (int i = 0; i < poptions.getOrder().length; i++) {
				if (i > 0)
					query.append(", ");
				query.append("A.ld_" + poptions.getOrder()[i]);
			}
		}
	}

	private Collection<Long> findAccessibleFolderIds(User user) {
		Collection<Long> ids = new HashSet<Long>();
		FolderDAO folderDAO = (FolderDAO) Context.get().getBean(FolderDAO.class);

		// Check if there is a folder specification in the criteria
		if (((FolderSearchOptions) options).getCriteria() != null)
			for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria()) {
				if (criterion.getType() == FolderCriterion.TYPE_FOLDER) {
					if (!criterion.isEmpty()) {
						if (FolderCriterion.OPERATOR_INORSUBFOLDERS.equals(criterion.getOperator())) {
							ids.addAll(folderDAO.findFolderIdByUserIdInPath(user.getId(), criterion.getLongValue()));
						} else if (folderDAO.isReadEnabled(criterion.getLongValue(), user.getId())) {
							ids.add(criterion.getLongValue());
						}
					}
				}
			}

		/*
		 * In case of normal user and without a folder criterion, we have to
		 * collect all accessible folders.
		 */
		if (ids.isEmpty() && !user.isMemberOf("admin"))
			ids = folderDAO.findFolderIdByUserIdInPath(options.getUserId(), null);

		return ids;
	}

	public class HitMapper implements RowMapper<Hit> {

		public Hit mapRow(ResultSet rs, int rowNum) throws SQLException {
			Hit hit = new Hit();
			hit.setId(rs.getLong(1));

			Folder folder = new Folder();
			folder.setId(rs.getLong(2));
			folder.setName(rs.getString(3));
			folder.setType(rs.getInt(7));
			hit.setFolder(folder);
			hit.setFileName(rs.getString(3));
			if (rs.getLong(8) != 0) {
				hit.setDocRef(rs.getLong(8));
				folder.setFoldRef(rs.getLong(8));
				hit.setType("folderalias");
			} else {
				hit.setType("folder");
			}
			hit.setCustomId(Long.toString(rs.getLong(1)));
			hit.setDate(rs.getTimestamp(6));
			hit.setCreation(rs.getTimestamp(5));
			hit.setComment(rs.getString(4));
			hit.setPublished(1);

			if (rs.getLong(10) != 0) {
				hit.setTemplateId(rs.getLong(10));
				hit.setTemplateName(rs.getString(9));
				Template t = new Template();
				t.setId(rs.getLong(10));
				t.setName(rs.getString(9));
				hit.setTemplate(t);
			}

			return hit;
		}
	};
}