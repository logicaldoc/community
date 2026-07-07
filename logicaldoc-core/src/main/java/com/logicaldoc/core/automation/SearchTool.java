package com.logicaldoc.core.automation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.HitField;
import com.logicaldoc.core.searchengine.Hits;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.Search.HitMapper;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.searchengine.SearchOptions;

/**
 * Utility methods to do searches from within Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
@AutomationDictionary
public class SearchTool {

    private static final Logger log = LoggerFactory.getLogger(SearchTool.class);

    /**
     * Factory method for searches.
     * 
     * @param options the search criteria
     * 
     * @return a {@link Search} instance for the given search options
     */
    public Search newSearch(SearchOptions options) {
        return Search.get(options);
    }

    /**
     * Instantiates a new {@link Search}
     * 
     * @param options the search criteria
     * 
     * @return The list of hits that satisfy the search criteria
     * 
     * @throws SearchException Raised in case of an error during the search
     */
    public List<Hit> search(SearchOptions options) throws SearchException {
        return newSearch(options).search();
    }

    /**
     * Executes a search directly in the full-text index
     * 
     * @param expression the search expression
     * @param tenantId the tenant
     * 
     * @param expressionLanguage the language of the expression
     * 
     * @return the hits
     * 
     * @since 8.7.4
     */
    public List<Hit> search(long tenantId, String expression, String expressionLanguage) {
        Set<String> filters = new HashSet<>();
        filters.add("%s:%d".formatted(HitField.TENANT_ID.getName(), tenantId));
        return search(tenantId, expression, filters, expressionLanguage);
    }

    /**
     * Executes a search directly in the full-text index
     * 
     * @param expression the search expression
     * @param filters some optional filters
     * @param tenantId the tenant
     * 
     * @param expressionLanguage the language of the expression
     * 
     * @return the hits
     * 
     * @since 8.7.4
     */
    public List<Hit> search(long tenantId, String expression, Set<String> filters, String expressionLanguage) {
        filters.add("%s:%d".formatted(HitField.TENANT_ID.getName(), tenantId));

        Hits result = SearchEngine.get().search(expression, filters, expressionLanguage, null);

        Map<Long, Hit> hitsMap = new HashMap<>();
        while (result.hasNext()) {
            Hit hit = result.next();
            hitsMap.put(hit.getId(), hit);
        }

        List<Hit> hits = new ArrayList<>();
        if (!hitsMap.isEmpty()) {

            Set<Long> hitsIds = hitsMap.keySet();
            StringBuilder hitsIdsCondition = new StringBuilder();
            if (!hitsIds.isEmpty()) {
                hitsIdsCondition.append(" and (");

                FolderDAO fdao = FolderDAO.get();
                if (fdao.isOracle()) {
                    /*
                     * In Oracle The limit of 1000 elements applies to sets of
                     * single items: (x) IN ((1), (2), (3), ...). There is no
                     * limit if the sets contain two or more items: (x, 0) IN
                     * ((1,0), (2,0), (3,0), ...):
                     */
                    hitsIdsCondition.append(" (A.ld_id,0) in ( ");
                    hitsIdsCondition.append(hitsIds.stream().map("(%d,0)"::formatted).collect(Collectors.joining(",")));
                } else {
                    hitsIdsCondition.append(" A.ld_id in ( ");
                    hitsIdsCondition
                            .append(hitsIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")));
                }
                hitsIdsCondition.append(" ) )");
            }

            // Find real documents
            StringBuilder richQuery = new StringBuilder(
                    """
                             select A.ld_id, A.ld_customid, A.ld_docref, A.ld_type, A.ld_version, A.ld_lastmodified,
                                    A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable
                                    A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type
                                    A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing
                                    A.ld_stoppublishing, A.ld_published, FOLD.ld_name, A.ld_folderid, A.ld_tgs tags,
                                    A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, A.ld_stamped, A.ld_password,
                                    A.ld_workflowstatusdisp, A.ld_language, A.ld_pages, A.ld_color
                               from ld_document A
                               join ld_folder FOLD on A.ld_folderid = FOLD.ld_id
                    left outer join ld_template C on A.ld_templateid = C.ld_id
                              where A.ld_deleted = 0
                                and A.ld_tenantid =
                                """);
            richQuery.append(Long.toString(tenantId));
            richQuery.append(hitsIdsCondition.toString());

            DocumentDAO dao = DocumentDAO.get();
            try {
                dao.query(richQuery.toString(), new HitMapper(hitsMap), null);
            } catch (Exception e) {
                log.error("Cannot enrich the hits", e);
            }

            // Now sort the hits by score desc
            hits = new ArrayList<>(hitsMap.values());
            try {
                Collections.sort(hits);
            } catch (Exception t) {
                log.warn(t.getMessage());
            }
        }

        return hits;
    }
}