package com.logicaldoc.core.searchengine;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;

/**
 * Search specialization for the Full text search.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class FulltextSearch extends Search {

    protected FulltextSearch() {
    }

    @Override
    public void internalSearch() throws SearchException {
        FulltextSearchOptions opt = (FulltextSearchOptions) options;

        setDefaultFields(opt);

        /*
         * Prepare the query: the expression must be applied to all requested
         * fields.
         */
        StringBuilder query = prepareQuery(opt);

        Collection<Long> accessibleFolderIds = getAccessibleFolderIds();

        long tenantId = getTenant(opt);

        /*
         * Prepare the filters
         */
        Set<String> filters = new HashSet<>();
        try {
            setQueryFilters(opt, filters, tenantId, accessibleFolderIds);
        } catch (PersistenceException e) {
            throw new SearchException(e);
        }

        /*
         * Launch the search
         */
        log.debug("Full-text seach: {}   filters: {}", query, filters);
        Hits results = SearchEngine.get().search(query.toString(), filters, opt.getExpressionLanguage(), null);
        log.debug("End of Full-text search");
        log.debug("Fulltext hits count: {}", (results != null ? results.getCount() : 0));

        // Save here the binding between ID and Hit
        Map<Long, Hit> hitsMap = buildHitsMap(opt, results);

        if (hitsMap.isEmpty())
            return;

        estimatedHitsNumber = results != null ? results.getEstimatedCount() : 0;

        log.debug("DB search");

        enrichAndPopulateHits(opt, hitsMap, tenantId, accessibleFolderIds);
    }

    private Map<Long, Hit> buildHitsMap(FulltextSearchOptions opt, Hits results) {
        Map<Long, Hit> hitsMap = new HashMap<>();
        while (results != null && results.hasNext()) {
            Hit hit = results.next();
            // Skip a document if not in the filter set
            if (opt.getFilterIds() != null && !opt.getFilterIds().isEmpty()
                    && !opt.getFilterIds().contains(hit.getId()))
                continue;
            hitsMap.put(hit.getId(), hit);
        }
        return hitsMap;
    }

    private void setQueryFilters(
            FulltextSearchOptions fulltextOptions,
            Set<String> filters,
            long tenantId,
            Collection<Long> accessibleFolderIds) throws SearchException, PersistenceException {
        if (searchUser != null && TenantDAO.get().count() > 1)
            filters.add("%s:%s%d".formatted(HitField.TENANT_ID, tenantId < 0 ? "\\" : "", tenantId));

        if (fulltextOptions.getTemplate() != null)
            filters.add("%s:%s%d".formatted(HitField.TEMPLATE_ID, fulltextOptions.getTemplate() < 0 ? "\\" : "",
                    fulltextOptions.getTemplate()));

        if (StringUtils.isNotEmpty(fulltextOptions.getLanguage()))
            filters.add("%s:%s".formatted(HitField.LANGUAGE, fulltextOptions.getLanguage()));

        if (fulltextOptions.getSizeMin() != null)
            filters.add("%s:[%d TO *]".formatted(HitField.SIZE, fulltextOptions.getSizeMin()));

        if (fulltextOptions.getSizeMax() != null)
            filters.add("%s:[* TO %d]".formatted(HitField.SIZE, fulltextOptions.getSizeMax()));

        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
        if (fulltextOptions.getDateFrom() != null)
            filters.add("%s:[%sT00:00:00Z TO *]".formatted(HitField.DATE, df.format(fulltextOptions.getDateFrom())));

        if (fulltextOptions.getDateTo() != null)
            filters.add("%s:[* TO %sT00:00:00Z]".formatted(HitField.DATE, df.format(fulltextOptions.getDateTo())));

        if (fulltextOptions.getCreationFrom() != null)
            filters.add("%s:[%sT00:00:00Z TO *]".formatted(HitField.CREATION, df.format(fulltextOptions.getCreationFrom())));

        if (fulltextOptions.getCreationTo() != null)
            filters.add("%s:[* TO %sT00:00:00Z]".formatted(HitField.CREATION, df.format(fulltextOptions.getCreationTo())));

        appendFolderQueryFilter(fulltextOptions, filters, accessibleFolderIds);
    }

    private void appendFolderQueryFilter(
            FulltextSearchOptions opt,
            Set<String> filters,
            Collection<Long> accessibleFolderIds) throws SearchException {
        FolderDAO fdao = FolderDAO.get();
        try {
            if (opt.getFolderId() != null && !accessibleFolderIds.contains(opt.getFolderId())
                    && fdao.isReadAllowed(opt.getFolderId().longValue(), opt.getUserId()))
                accessibleFolderIds.add(opt.getFolderId());
        } catch (PersistenceException e1) {
            throw new SearchException(e1.getMessage(), e1);
        }

        StringBuilder foldersFilter = new StringBuilder();
        if (!accessibleFolderIds.isEmpty() && opt.getFolderId() != null) {
            for (Long id : accessibleFolderIds) {
                if (!foldersFilter.isEmpty())
                    foldersFilter.append(" or ");
                foldersFilter.append(HitField.FOLDER_ID);
                foldersFilter.append(":");
                foldersFilter.append(id < 0 ? "\\" : "");
                foldersFilter.append(Long.toString(id));
            }

            filters.add(" (%s) ".formatted(foldersFilter.toString()));
        }
    }

    private long getTenant(FulltextSearchOptions opt) {
        long tenantId = Tenant.DEFAULT_ID;
        if (opt.getTenantId() != null)
            tenantId = opt.getTenantId().longValue();
        else if (searchUser != null)
            tenantId = searchUser.getTenantId();
        return tenantId;
    }

    private StringBuilder prepareQuery(FulltextSearchOptions opt) {
        StringBuilder query = new StringBuilder();
        for (String field : opt.getFields()) {
            if (!query.isEmpty())
                query.append(" OR ");

            query.append(field);
            query.append(":(");
            query.append(opt.getExpression());
            query.append(")");
        }
        return query;
    }

    private void setDefaultFields(FulltextSearchOptions opt) {
        if (opt.getFields().isEmpty()) {
            opt.setFields(new HashSet<>(Arrays.asList(HitField.FILENAME.toString(), HitField.TITLE.toString(),
                    HitField.TAGS.toString(), HitField.CONTENT.toString())));
        }
    }
}