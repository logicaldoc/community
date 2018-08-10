package com.logicaldoc.core.searchengine.folder;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
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

	/**
	 * Retrieve the maximum number of folder ids to use in a query (in the IN
	 * clause).
	 */
	private long getQueryMaxFolderIds() {
		ContextProperties config = Context.get().getProperties();
		long max = config.getLong("query.maxfolderids", Long.MAX_VALUE);
		if (max <= 0)
			max = Long.MAX_VALUE;
		return max;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void internalSearch() throws Exception {
		Collection<Long> accessibleFolderIds = findAccessibleFolderIds();
		Object[] params = prepareExpression(accessibleFolderIds);
		options.setParameters(params);

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		// Execute the search
		List<Hit> folders = (List<Hit>) dao.query(options.getExpression(), params, new HitMapper(),
				options.getMaxHits());

		if (accessibleFolderIds.size() <= getQueryMaxFolderIds()) {
			// Security checks were already considered in the search query
			hits = folders;
		} else {
			// Due to high IDs number we have to apply security checks
			hits.clear();

			// Filter the folders with the accessible IDs
			hits = folders.stream().filter(f -> accessibleFolderIds.contains(f.getId())).collect(Collectors.toList());
		}

		moreHitsPresent = hits.size() >= options.getMaxHits();
		if (moreHitsPresent)
			estimatedHitsNumber = hits.size() + 1;
		else
			estimatedHitsNumber = hits.size();
	}

	/**
	 * Utility method that prepares the query expression
	 */
	private Object[] prepareExpression(Collection<Long> accessibleFolderIds) {
		if (StringUtils.isNotEmpty(options.getExpression()))
			return options.getParameters();

		ArrayList<Object> params = new ArrayList<Object>();

		StringBuffer query = new StringBuffer();

		if (options.getRetrieveAliases() == 1)
			query.append("(");

		// Find all real folders
		query.append("select A.ld_id, A.ld_parentid, A.ld_name, A.ld_description, A.ld_creation, A.ld_lastmodified, A.ld_type, A.ld_foldref, C.ld_name, A.ld_templateid ");
		query.append(" from ld_folder A ");
		query.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");

		appendWhereClause(false, params, query, accessibleFolderIds);

		if (options.getRetrieveAliases() == 1) {
			// Append all aliases
			query.append(") UNION (select A.ld_id, A.ld_parentid, A.ld_name, REF.ld_description, A.ld_creation, A.ld_lastmodified, A.ld_type, A.ld_foldref, C.ld_name, A.ld_templateid ");
			query.append(" from ld_folder A ");
			query.append(" join ld_folder REF on A.ld_foldref=REF.ld_id ");
			query.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");
			appendWhereClause(true, params, query, accessibleFolderIds);
			query.append(")");
		}

		options.setExpression(query.toString());

		log.info("executing query=" + query.toString());
		log.info("with parameters=" + params);

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
	private void appendWhereClause(boolean searchAliases, ArrayList<Object> params, StringBuffer query,
			Collection<Long> accessibleFolderIds) {
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
					if (!criterion.isNull()) {
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
		if (composition != null && ((FolderSearchOptions) options).getCriteria() != null)
			for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria())
				criterion.setComposition(composition);

		if (searchAliases)
			query.append(" and A.ld_foldref is not null and REF.ld_deleted=0 and A.ld_foldref = REF.ld_id ");
		else
			query.append(" and A.ld_foldref is null ");

		/*
		 * Filter on the folders the user can access
		 */
		if (!accessibleFolderIds.isEmpty() && accessibleFolderIds.size() <= getQueryMaxFolderIds()) {
			/*
			 * This approach pre-calculate the accessible IDs and puts them in
			 * IN clauses.<br /> Oracle cannot handle more than 1000 elements in
			 * a single in clause so the whole list is partitioned
			 */
			query.append(" and ( ");
			List<List<Long>> folderId = org.apache.commons.collections4.ListUtils.partition(new ArrayList<Long>(
					accessibleFolderIds), 1000);
			int i = 0;
			for (List<Long> list : folderId) {
				if (i > 0)
					query.append(" or ");
				query.append(" A.ld_id in (");
				query.append(list.toString().replace('[', ' ').replace(']', ' '));
				query.append(") ");
				i++;
			}
			query.append(" ) ");
		}

		// Now add all criteria
		boolean first = true;
		counter = 0;

		if (options != null && ((FolderSearchOptions) options).getCriteria() != null)
			for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria()) {
				if (criterion.isNull() || criterion.getType() == FolderCriterion.TYPE_FOLDER)
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
					query.append(" and C" + counter + ".ld_name='");
					query.append(criterion.getFieldName());
					query.append("' and ");
					columnName = "C" + counter + ".";
					switch (criterion.getType()) {
					case Attribute.TYPE_STRING:
						columnName += "ld_stringvalue";
						break;
					case Attribute.TYPE_INT:
						columnName += "ld_intvalue";
						break;
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
						if (FolderCriterion.OPERATOR_EQUAL.equals(criterion.getOperator()))
							query.append(columnName + " = '" + val + "'");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not " + columnName + " = '" + val + "')");
						else if ("contains".equals(criterion.getOperator()))
							query.append(columnName + " like '%" + val + "%'");
						else if ("notcontains".equals(criterion.getOperator()))
							query.append(" (" + columnName + " is null or not (" + columnName + " like '%" + val
									+ "%'))");
					} else {
						String val = SqlUtil.doubleQuotes(criterion.getStringValue().toLowerCase());
						if (FolderCriterion.OPERATOR_EQUAL.equals(criterion.getOperator()))
							query.append("lower(" + columnName + ") = '" + val + "'");
						else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
							query.append("(not lower(" + columnName + ") = '" + val + "')");
						else if ("contains".equals(criterion.getOperator()))
							query.append("lower(" + columnName + ") like '%" + val + "%'");
						else if ("notcontains".equals(criterion.getOperator()))
							query.append(" (" + columnName + " is null or not (lower(" + columnName + ") like '%" + val
									+ "%')");
						break;
					}
					break;
				case Attribute.TYPE_INT:
					params.add(criterion.getLongValue());
					if (FolderCriterion.OPERATOR_EQUAL.equals(criterion.getOperator()))
						query.append(columnName + " = ?");
					else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
						query.append("(not " + columnName + " = ?)");
					else if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
						query.append(columnName + " > ?");
					else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
						query.append(columnName + " < ?");
					break;
				case Attribute.TYPE_BOOLEAN:
					if (criterion.getLongValue() == null)
						params.add(null);
					else
						params.add(criterion.getLongValue());
					query.append(columnName + " = ?");
					break;
				case Attribute.TYPE_DOUBLE:
					params.add(criterion.getDoubleValue());
					if (FolderCriterion.OPERATOR_EQUAL.equals(criterion.getOperator()))
						query.append(columnName + " = ?");
					else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
						query.append("(not " + columnName + " = ?)");
					else if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
						query.append(columnName + " > ?");
					else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
						query.append(columnName + " < ?");
					break;
				case Attribute.TYPE_DATE:
					params.add(criterion.getSqlDateValue());
					if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
						query.append(columnName + " > ?");
					else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
						query.append(columnName + " < ?");
					break;
				case FolderCriterion.TYPE_TEMPLATE:
					params.add(criterion.getLongValue());
					if (FolderCriterion.OPERATOR_EQUAL.equals(criterion.getOperator()))
						query.append(columnName + " = ?");
					else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
						query.append("(not " + columnName + " = ?)");
					break;
				case FolderCriterion.TYPE_LANGUAGE:
					String val2 = criterion.getStringValue();
					if (FolderCriterion.OPERATOR_EQUAL.equals(criterion.getOperator()))
						query.append(columnName + " = '" + val2 + "'");
					else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
						query.append("(not " + columnName + " = '" + val2 + "')");
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

	private Collection<Long> findAccessibleFolderIds() {
		UserDAO userDAO = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDAO.findById(options.getUserId());
		userDAO.initialize(user);

		Collection<Long> ids = new HashSet<Long>();
		FolderDAO folderDAO = (FolderDAO) Context.get().getBean(FolderDAO.class);

		// Check if there is a folder specification in the criteria
		if (((FolderSearchOptions) options).getCriteria() != null)
			for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria()) {
				if (criterion.getType() == FolderCriterion.TYPE_FOLDER) {
					if (!criterion.isNull()) {
						if (FolderCriterion.OPERATOR_INORSUBFOLDERS.equals(criterion.getOperator())) {
							ids.addAll(folderDAO.findFolderIdByUserId(user.getId(), criterion.getLongValue(), true));
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
			ids = folderDAO.findFolderIdByUserId(options.getUserId(), null, true);

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
			
			if (rs.getLong(10) != 0){
				hit.setTemplateId(rs.getLong(10));
				hit.setTemplateName(rs.getString(9));
				Template t=new Template();
				t.setId(rs.getLong(10));
				t.setName(rs.getString(9));
				hit.setTemplate(t);
			}

			return hit;
		}
	};
}