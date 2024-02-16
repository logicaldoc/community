package com.logicaldoc.core.automation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.searchengine.FulltextSearch.HitMapper;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.HitField;
import com.logicaldoc.core.searchengine.Hits;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.util.Context;

/**
 * Utility methods to do searches from within Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
@AutomationDictionary
public class SearchTool {

	protected static Logger log = LoggerFactory.getLogger(SearchTool.class);

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
		return search(tenantId, expression, Set.of(HitField.TENANT_ID.getName() + ":" + tenantId), expressionLanguage);
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
		filters.add(HitField.TENANT_ID.getName() + ":" + tenantId);

		SearchEngine engine = (SearchEngine) Context.get().getBean(SearchEngine.class);
		Hits result = engine.search(expression, filters, expressionLanguage, null);

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

				FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				if (fdao.isOracle()) {
					/*
					 * In Oracle The limit of 1000 elements applies to sets of
					 * single items: (x) IN ((1), (2), (3), ...). There is no
					 * limit if the sets contain two or more items: (x, 0) IN
					 * ((1,0), (2,0), (3,0), ...):
					 */
					hitsIdsCondition.append(" (A.ld_id,0) in ( ");
					hitsIdsCondition
							.append(hitsIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
					hitsIdsCondition.append(" )");
				} else {
					hitsIdsCondition.append(" A.ld_id in " + hitsIds.toString().replace('[', '(').replace(']', ')'));
				}

				hitsIdsCondition.append(")");
			}

			// Find real documents
			StringBuilder richQuery = new StringBuilder(
					"select A.ld_id, A.ld_customid, A.ld_docref, A.ld_type, A.ld_version, A.ld_lastmodified, ");
			richQuery
					.append(" A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable, ");
			richQuery.append(" A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type, ");
			richQuery.append(
					" A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing, ");
			richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
			richQuery.append(
					" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
			richQuery.append(
					" A.ld_stamped, A.ld_password, A.ld_workflowstatusdisp, A.ld_language, A.ld_pages, A.ld_color ");
			richQuery.append(" from ld_document A ");
			richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
			richQuery.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");
			richQuery.append(" where A.ld_deleted=0 and A.ld_tenantid = " + tenantId);
			richQuery.append(hitsIdsCondition.toString());

			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
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