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

	private static final String COLON_STAR_TO = ":[* TO ";

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

	private void setQueryFilters(FulltextSearchOptions fulltextOptions, Set<String> filters, long tenantId,
			Collection<Long> accessibleFolderIds) throws SearchException, PersistenceException {
		if (searchUser != null && TenantDAO.get().count() > 1)
			filters.add(HitField.TENANT_ID + ":" + (tenantId < 0 ? "\\" : "") + tenantId);

		if (fulltextOptions.getTemplate() != null)
			filters.add(HitField.TEMPLATE_ID + ":" + (fulltextOptions.getTemplate() < 0 ? "\\" : "")
					+ fulltextOptions.getTemplate());

		if (StringUtils.isNotEmpty(fulltextOptions.getLanguage()))
			filters.add(HitField.LANGUAGE + ":" + fulltextOptions.getLanguage());

		if (fulltextOptions.getSizeMin() != null)
			filters.add(HitField.SIZE + ":[" + fulltextOptions.getSizeMin() + " TO *]");

		if (fulltextOptions.getSizeMax() != null)
			filters.add(HitField.SIZE + COLON_STAR_TO + fulltextOptions.getSizeMax() + "]");

		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		if (fulltextOptions.getDateFrom() != null)
			filters.add(HitField.DATE + ":[" + df.format(fulltextOptions.getDateFrom()) + "T00:00:00Z TO *]");

		if (fulltextOptions.getDateTo() != null)
			filters.add(HitField.DATE + COLON_STAR_TO + df.format(fulltextOptions.getDateTo()) + "T00:00:00Z]");

		if (fulltextOptions.getCreationFrom() != null)
			filters.add(HitField.CREATION + ":[" + df.format(fulltextOptions.getCreationFrom()) + "T00:00:00Z TO *]");

		if (fulltextOptions.getCreationTo() != null)
			filters.add(HitField.CREATION + COLON_STAR_TO + df.format(fulltextOptions.getCreationTo()) + "T00:00:00Z]");

		appendFolderQueryFilter(fulltextOptions, filters, accessibleFolderIds);
	}

	private void appendFolderQueryFilter(FulltextSearchOptions opt, Set<String> filters,
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
				if (foldersFilter.length() > 0)
					foldersFilter.append(" or ");
				foldersFilter.append(HitField.FOLDER_ID + ":" + (id < 0 ? "\\" : "") + id);
			}

			filters.add(" (" + foldersFilter.toString() + ") ");
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
			if (query.length() > 0)
				query.append(" OR ");

			query.append(field + ":(" + opt.getExpression() + ")");
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