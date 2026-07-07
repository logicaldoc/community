package com.logicaldoc.core.searchengine.folder;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

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
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Search specialization for Folder searches.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FolderSearch extends Search {

    private static final String AND = " and ";

    private String query;

    @Override
    public void internalSearch() throws SearchException {
        UserDAO userDAO = UserDAO.get();
        User user;
        try {
            user = userDAO.findById(options.getUserId());
            userDAO.initialize(user);
        } catch (PersistenceException e1) {
            throw new SearchException(e1);
        }

        Map<String, Object> params = null;
        try {
            params = prepareQuery();
        } catch (PersistenceException e1) {
            throw new SearchException(e1);
        }

        options.setParameters(params);

        FolderDAO dao = FolderDAO.get();
        // Execute the search
        List<Hit> folders;
        try {
            folders = dao.query(query, params, new HitMapper(), null);
        } catch (PersistenceException e) {
            throw new SearchException(e);
        }

        estimatedHitsNumber = folders.size();

        // Traverse the results checking visibility and count
        Collection<Long> accessibleFolderIds;
        try {
            accessibleFolderIds = findAccessibleFolderIds(user);
        } catch (PersistenceException e) {
            throw new SearchException(e.getMessage(), e);
        }
        for (Hit folder : folders) {
            if (accessibleFolderIds.contains(folder.getId()) || user.isMemberOf(Group.GROUP_ADMIN))
                hits.add(folder);
            if (hits.size() >= options.getMaxHits())
                break;
        }

        if (hits.size() < options.getMaxHits())
            estimatedHitsNumber = hits.size();
    }

    /**
     * Utility method that prepares the query expression
     * 
     * PersistenceException error at data layer
     */
    private Map<String, Object> prepareQuery() throws PersistenceException {
        if (StringUtils.isNotEmpty(query))
            return options.getParameters();

        Map<String, Object> params = new HashMap<>();
        StringBuilder sb = new StringBuilder();

        if (options.isRetrieveAliases())
            sb.append("(");

        // Find all real folders
        sb.append(
                "select A.ld_id, A.ld_parentid, A.ld_name, A.ld_description, A.ld_creation, A.ld_lastmodified, A.ld_type, A.ld_foldref, C.ld_name, A.ld_templateid, A.ld_tgs, A.ld_color ");
        sb.append(" from ld_folder A ");
        sb.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");
        appendWhereClause(false, params, sb);

        if (options.isRetrieveAliases()) {
            // Append all aliases
            sb.append(
                    ") UNION (select A.ld_id, A.ld_parentid, A.ld_name, A.ld_description, A.ld_creation, A.ld_lastmodified, A.ld_type, A.ld_foldref, C.ld_name, REF.ld_templateid, REF.ld_tgs, A.ld_color ");
            sb.append(" from ld_folder A ");
            sb.append(" join ld_folder REF on A.ld_foldref=REF.ld_id ");
            sb.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");
            appendWhereClause(true, params, sb);
            sb.append(")");
        }

        log.info("executing query {} with parameters {}", sb, params);
        query = sb.toString();
        return params;
    }

    /**
     * This method appends the where clause considering or not the aliases on
     * the search.
     * 
     * @param searchAliases If true, also the aliases must be considered in the
     *        search
     * @param params the query parameters map
     * @param query the query
     * 
     * @throws PersistenceException error at data layer
     */
    private void appendWhereClause(boolean searchAliases, Map<String, Object> params, StringBuilder query)
            throws PersistenceException {
        FolderSearchOptions fOptions = (FolderSearchOptions) options;

        /*
         * Prepare the joins for the extended attributes. If the TOP condition
         * is AND we have to do multiple joins(one per criteria), otherwise if
         * the TOP operator is OR we have to use just one join to avoid high
         * load on the database.
         */
        prepareExtendedAttributesJoins(query);

        query.append(" where A.ld_deleted = 0 and A.ld_hidden = 0 ");
        query.append(AND);
        if (!searchAliases)
            query.append(" not ");
        query.append("A.ld_type = %d ".formatted(Folder.TYPE_ALIAS));

        appendTenantCondition(query);

        String tableAlias = getTableAlias(searchAliases);

        appendTemplateCondition(query, tableAlias);

        appendMainFolderCondition(query, tableAlias, fOptions);

        appendComposition(fOptions);

        appendFoldRefConfition(query, searchAliases);

        // Now add all the criteria
        String criteriaQueryPart = prepareCriteriaConditions(tableAlias, fOptions, params);

        if (StringUtils.isNotEmpty(criteriaQueryPart)) {
            query.append(" and ( ");
            if ("not".equals(fOptions.getTopOperator()))
                query.append("not");
            query.append(criteriaQueryPart);
            query.append(")");
        }

        appendSorting(query, fOptions);
    }

    private String prepareCriteriaConditions(
            String tableAlias,
            FolderSearchOptions fOptions,
            Map<String, Object> params) throws PersistenceException {
        StringBuilder criteriaQueryPart = new StringBuilder();
        int joinsCounter = 0;

        for (FolderCriterion criterion : fOptions.getNotEmptyCriteria()) {
            if (!criteriaQueryPart.isEmpty()) {
                criteriaQueryPart.append(" ");
                criteriaQueryPart.append(criterion.getComposition());
            }
            criteriaQueryPart.append("(");

            String columnName = "";
            if (criterion.isExtendedAttribute()) {
                // In case of OR top operator we only have a single join
                // with extended attributes
                if (!"or".equals(fOptions.getTopOperator()) || joinsCounter == 0)
                    joinsCounter++;
                criteriaQueryPart.append("(C%d.ld_folderid = %s.ld_id".formatted(joinsCounter, tableAlias));
                criteriaQueryPart.append(" and (C%d.ld_name='".formatted(joinsCounter));
                criteriaQueryPart.append(criterion.getFieldName());
                criteriaQueryPart.append("' or C%d.ld_name like '".formatted(joinsCounter));
                criteriaQueryPart.append(criterion.getFieldName());
                criteriaQueryPart.append("-%') and ");
                columnName = "C%d.".formatted(joinsCounter);
                switch (criterion.getType()) {
                    case Attribute.TYPE_INT, Attribute.TYPE_USER, Attribute.TYPE_FOLDER, Attribute.TYPE_DOCUMENT, Attribute.TYPE_BOOLEAN:
                        columnName += "ld_intvalue";
                        break;
                    case Attribute.TYPE_DOUBLE:
                        columnName += "ld_doublevalue";
                        break;
                    case Attribute.TYPE_DATE:
                        columnName += "ld_datevalue";
                        break;
                    default:
                        columnName += "ld_stringvalue";
                        break;
                }
            } else {
                columnName = getCriterionColumnName(tableAlias, criterion);
            }

            appendAttributeCriterionColumnCondition(criteriaQueryPart, tableAlias, columnName, criterion, params);

            if (criterion.isExtendedAttribute())
                criteriaQueryPart.append(")");
            criteriaQueryPart.append(")");
        }
        return criteriaQueryPart.toString();
    }

    private String getCriterionColumnName(String tableAlias, FolderCriterion criterion) {
        String columnName;
        columnName = criterion.getColumnName();

        // If the column name is not qualified, prepend the current
        // table alias
        if (!columnName.contains("."))
            columnName = "%s.%s".formatted(tableAlias, columnName);
        return columnName;
    }

    private String getTableAlias(boolean searchAliases) {
        String tableAlias = "A";
        if (searchAliases)
            tableAlias = "REF";
        return tableAlias;
    }

    private void appendSorting(StringBuilder query, FolderSearchOptions fOptions) {
        if (fOptions.getOrder() != null && !fOptions.getOrder().isEmpty()) {
            query.append(" order by ");
            query.append(fOptions.getOrder().stream().map("A.ld_%s"::formatted).collect(Collectors.joining(", ")));
        }
    }

    private void appendTemplateCondition(StringBuilder query, String tableAlias) {
        if (options.getTemplate() != null) {
            query.append(AND);
            query.append(tableAlias);
            query.append(".ld_templateid = ");
            query.append(Long.toString(options.getTemplate()));
        }
    }

    private void appendFoldRefConfition(StringBuilder query, boolean searchAliases) {
        if (searchAliases)
            query.append(" and A.ld_foldref is not null and REF.ld_deleted = 0 and A.ld_foldref = REF.ld_id ");
        else
            query.append(" and A.ld_foldref is null ");
    }

    private void appendMainFolderCondition(StringBuilder query, String tableAlias, FolderSearchOptions fOptions)
            throws PersistenceException {
        if (fOptions.getFolderId() != null) {
            query.append(AND);
            query.append(tableAlias);
            if (fOptions.isSearchInSubPath()) {
                query.append(".ld_path like '");
                query.append(FolderDAO.get().computePath(fOptions.getFolderId()));
                query.append("/%'");
            } else {
                query.append(".ld_parentid = ");
                query.append(Long.toString(fOptions.getFolderId()));
            }
        }
    }

    private void appendAttributeCriterionColumnCondition(
            StringBuilder query,
            String tableAlias,
            String columnName,
            FolderCriterion criterion,
            Map<String, Object> params) throws PersistenceException {
        switch (criterion.getType()) {
            case Attribute.TYPE_INT:
                appendIntegerCriterion(query, columnName, criterion, params);
                break;
            case Attribute.TYPE_FOLDER, Attribute.TYPE_DOCUMENT, Attribute.TYPE_USER, FolderCriterion.TYPE_TEMPLATE:
                appendFolderOrDocumentOrUserOrTemplateCriterion(query, columnName, criterion, params);
                break;
            case Attribute.TYPE_BOOLEAN:
                appendBooleanCriterion(query, columnName, criterion, params);
                break;
            case Attribute.TYPE_DOUBLE:
                appendDoubleCriterion(query, columnName, criterion, params);
                break;
            case Attribute.TYPE_DATE:
                appendDateCriterion(query, columnName, criterion, params);
                break;
            case FolderCriterion.TYPE_LANGUAGE:
                appendLanguageCriterion(query, columnName, criterion);
                break;
            case FolderCriterion.TYPE_FOLDER:
                appendFolderCriterion(query, tableAlias, columnName, criterion);
                break;
            default:
                if (options.isCaseSensitive()) {
                    appendStringCriterionCaseSensitive(query, columnName, criterion);
                } else {
                    appendStringCriterionCaseInsensitive(query, columnName, criterion);
                }
                break;
        }
    }

    private void appendStringCriterionCaseInsensitive(
            StringBuilder query,
            String columnName,
            FolderCriterion criterion) {
        String val = SqlUtil.doubleQuotes(criterion.getStringValue().toLowerCase());
        if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
            query.append("lower(%s) = '%s'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
            query.append("(not lower(%s) = '%s')".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_CONTAINS.equals(criterion.getOperator()))
            query.append("lower(%s) like '%%%s%%'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_NOTCONTAINS.equals(criterion.getOperator()))
            query.append(" (%s is null or not (lower(%s) like '%%%s%%')".formatted(columnName, columnName, val));
        else if (FolderCriterion.OPERATOR_BEGINSWITH.equals(criterion.getOperator()))
            query.append("lower(%s) like '%s%%'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_ENDSWITH.equals(criterion.getOperator()))
            query.append("lower(%s) like  '%%%s'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator()))
            query.append(isNull(columnName));
        else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator()))
            query.append(isNotNull(columnName));
    }

    private void appendStringCriterionCaseSensitive(StringBuilder query, String columnName, FolderCriterion criterion) {
        String val = SqlUtil.doubleQuotes(criterion.getStringValue());
        if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
            query.append("%s = '%s'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
            query.append(" (not %s = '%s')".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_CONTAINS.equals(criterion.getOperator()))
            query.append("%s like '%%%s%%'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_NOTCONTAINS.equals(criterion.getOperator()))
            query.append(" (%s is null or not (%s like '%%%s%%'))".formatted(columnName, columnName, val));
        else if (FolderCriterion.OPERATOR_BEGINSWITH.equals(criterion.getOperator()))
            query.append("%s like '%s%%'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_ENDSWITH.equals(criterion.getOperator()))
            query.append("%s like '%%%s'".formatted(columnName, val));
        else if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator()))
            query.append(isNull(columnName));
        else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator()))
            query.append(isNotNull(columnName));
    }

    private void appendFolderCriterion(
            StringBuilder query,
            String tableAlias,
            String columnName,
            FolderCriterion criterion) throws PersistenceException {
        if (FolderCriterion.OPERATOR_INORSUBFOLDERS.equals(criterion.getOperator())) {
            FolderDAO dao = FolderDAO.get();
            String path = dao.computePath(criterion.getLongValue());
            query.append("%s.ld_path like '%s/%%'".formatted(tableAlias, path));
        } else
            query.append("%s = %d".formatted(columnName, criterion.getLongValue()));
    }

    private void appendLanguageCriterion(StringBuilder query, String columnName, FolderCriterion criterion) {
        if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
            query.append(isNull(columnName));
        } else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
            query.append(isNotNull(columnName));
        } else {
            String val2 = criterion.getStringValue();
            if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
                query.append("%s = '%s'".formatted(columnName, val2));
            else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
                query.append("(not %s = '%s')".formatted(columnName, val2));
        }
    }

    private void appendDateCriterion(
            StringBuilder query,
            String columnName,
            FolderCriterion criterion,
            Map<String, Object> params) {
        if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
            query.append(isNull(columnName));
        } else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
            query.append(isNotNull(columnName));
        } else {
            String paramName = addParameter(params, criterion.getSqlDateValue());
            if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
                query.append(greaterThan(columnName, paramName));
            else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
                query.append(lowerThan(columnName, paramName));
        }
    }

    private String addParameter(Map<String, Object> params, Object value) {
        String paramName = "param%d".formatted(params.size());
        params.put(paramName, value);
        return paramName;
    }

    private void appendDoubleCriterion(
            StringBuilder query,
            String columnName,
            FolderCriterion criterion,
            Map<String, Object> params) {
        if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
            query.append(isNull(columnName));
        } else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
            query.append(isNotNull(columnName));
        } else {
            String paramName = addParameter(params, criterion.getDoubleValue());
            if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
                query.append(equals(columnName, paramName));
            else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
                query.append(notEqual(columnName, paramName));
            else if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
                query.append(greaterThan(columnName, paramName));
            else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
                query.append(lowerThan(columnName, paramName));
        }
    }

    private void appendBooleanCriterion(
            StringBuilder query,
            String columnName,
            FolderCriterion criterion,
            Map<String, Object> params) {
        if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator())) {
            String paramName = addParameter(params, criterion.getLongValue());
            query.append(equals(columnName, paramName));
        } else if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
            query.append(isNull(columnName));
        } else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
            query.append(isNotNull(columnName));
        }
    }

    private void appendFolderOrDocumentOrUserOrTemplateCriterion(
            StringBuilder query,
            String columnName,
            FolderCriterion criterion,
            Map<String, Object> params) {
        if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
            query.append(isNull(columnName));
        } else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
            query.append(isNotNull(columnName));
        } else {
            String paramName = addParameter(params, criterion.getLongValue());
            if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator())) {
                query.append(equals(columnName, paramName));
            } else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
                query.append(notEqual(columnName, paramName));
        }
    }

    private void appendIntegerCriterion(
            StringBuilder query,
            String columnName,
            FolderCriterion criterion,
            Map<String, Object> params) {
        if (FolderCriterion.OPERATOR_NULL.equals(criterion.getOperator())) {
            query.append(isNull(columnName));
        } else if (FolderCriterion.OPERATOR_NOTNULL.equals(criterion.getOperator())) {
            query.append(isNotNull(columnName));
        } else {
            String paramName = addParameter(params, criterion.getLongValue());
            if (FolderCriterion.OPERATOR_EQUALS.equals(criterion.getOperator()))
                query.append(equals(columnName, paramName));
            else if (FolderCriterion.OPERATOR_NOTEQUAL.equals(criterion.getOperator()))
                query.append(notEqual(columnName, paramName));
            else if (FolderCriterion.OPERATOR_GREATER.equals(criterion.getOperator()))
                query.append(greaterThan(columnName, paramName));
            else if (FolderCriterion.OPERATOR_LESSER.equals(criterion.getOperator()))
                query.append(lowerThan(columnName, paramName));
        }
    }

    private String lowerThan(String columnName, String paramName) {
        return "%s < :%s".formatted(columnName, paramName);
    }

    private String greaterThan(String columnName, String paramName) {
        return "%s > :%s".formatted(columnName, paramName);
    }

    private String notEqual(String columnName, String paramName) {
        return "(not %s = :%s)".formatted(columnName, paramName);
    }

    private String equals(String columnName, String paramName) {
        return "%s = :%s".formatted(columnName, paramName);
    }

    private String isNotNull(String columnName) {
        return "%s is not null ".formatted(columnName);
    }

    private String isNull(String columnName) {
        return "%s is null ".formatted(columnName);
    }

    private void appendComposition(FolderSearchOptions fOptions) {
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
    }

    private void appendTenantCondition(StringBuilder query) {
        long tenantId = Tenant.DEFAULT_ID;
        if (options.getTenantId() != null)
            tenantId = options.getTenantId().longValue();
        else if (searchUser != null)
            tenantId = searchUser.getTenantId();
        query.append(" and A.ld_tenantid = %d".formatted(tenantId));
    }

    /**
     * Prepares the joins for the extended attributes. If the TOP condition is
     * AND we have to do multiple joins(one per criteria), otherwise if the TOP
     * operator is OR we have to use just one join to avoid high load on the
     * database.
     * 
     * @param query the current query
     */
    private void prepareExtendedAttributesJoins(StringBuilder query) {
        int counter = 0;
        if (((FolderSearchOptions) options).getCriteria() != null)
            for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria()) {
                if (criterion.isExtendedAttribute() && !criterion.isEmpty()) {
                    counter++;
                    query.append(", ld_folder_ext C");
                    query.append(Integer.toString(counter));
                    if ("or".equals(options.getTopOperator()))
                        break;
                }
            }
    }

    private Collection<Long> findAccessibleFolderIds(User user) throws PersistenceException {

        // Check if there are folder specifications in the criteria and use them
        Collection<Long> ids = new HashSet<>();
        if (((FolderSearchOptions) options).getCriteria() != null)
            ids = retrieveAccessibleFolderIdsFromFolderCriterions(user);

        /*
         * In case of normal user and without a folder criterion, we have to
         * collect all accessible folders.
         */
        if (ids.isEmpty() && !user.isMemberOf(Group.GROUP_ADMIN)) {
            FolderDAO folderDAO = FolderDAO.get();
            ids = folderDAO.findFolderIdByUserId(options.getUserId(), null, true);
        }

        return ids;
    }

    private Collection<Long> retrieveAccessibleFolderIdsFromFolderCriterions(User user) throws PersistenceException {
        FolderDAO folderDAO = FolderDAO.get();
        Collection<Long> ids = new HashSet<>();
        for (FolderCriterion criterion : ((FolderSearchOptions) options).getCriteria()) {
            if (criterion.getType() == FolderCriterion.TYPE_FOLDER && !criterion.isEmpty()) {
                if (FolderCriterion.OPERATOR_INORSUBFOLDERS.equals(criterion.getOperator())) {
                    ids.addAll(folderDAO.findFolderIdByUserIdInPath(user.getId(), criterion.getLongValue()));
                } else if (folderDAO.isReadAllowed(criterion.getLongValue(), user.getId())) {
                    ids.add(criterion.getLongValue());
                }
            }
        }
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
            hit.setPublished(true);

            if (rs.getLong(10) != 0) {
                hit.setTemplateId(rs.getLong(10));
                hit.setTemplateName(rs.getString(9));
                Template t = new Template();
                t.setId(rs.getLong(10));
                t.setName(rs.getString(9));
                hit.setTemplate(t);
            }

            hit.setTgs(rs.getString(11));

            if (hit.getTgs() != null) {
                StringTokenizer st = new StringTokenizer(hit.getTgs(), ",", false);
                while (st.hasMoreElements()) {
                    Object tag = st.nextElement();
                    if (tag != null && !tag.toString().isEmpty())
                        hit.addTag(tag.toString());
                }
            }

            hit.setColor(rs.getString(12));

            return hit;
        }
    }
}