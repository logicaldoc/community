package com.logicaldoc.core.searchengine;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.util.spring.Context;

/**
 * Search specialization for the Full text search.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class FulltextSearch extends Search {

	private static final String COLON_STAR_TO = ":[* TO ";

	public static class HitMapper implements RowMapper<Hit> {

		private Map<Long, Hit> hitsMap;

		public HitMapper(Map<Long, Hit> hitsMap) {
			super();
			this.hitsMap = hitsMap;
		}

		public Hit mapRow(ResultSet rs, int rowNum) throws SQLException {
			Hit hit = hitsMap.get(rs.getLong(1));
			if (hit == null) {
				// This is an alias
				hit = new Hit();
				hitsMap.put(rs.getLong(1), hit);
			}

			hit.setId(rs.getLong(1));
			hit.setCustomId(rs.getString(2));
			if (rs.getLong(3) != 0L) {
				hit.setDocRef(rs.getLong(3));
				Hit master = hitsMap.get(rs.getLong(3));
				if (master != null) {
					hit.setContent(master.getContent());
					hit.setSummary(master.getSummary());
				}
				hit.setDocRefType(rs.getString(33));
			}
			hit.setType(rs.getString(4));
			hit.setVersion(rs.getString(5));
			hit.setLastModified(rs.getTimestamp(6));
			hit.setDate(rs.getTimestamp(7));
			hit.setPublisher(rs.getString(8));
			hit.setCreation(rs.getTimestamp(9));
			hit.setCreator(rs.getString(10));
			hit.setFileSize(rs.getLong(11));
			hit.setImmutable(rs.getInt(12));
			hit.setIndexingStatus(rs.getInt(13));
			hit.setLockUserId(rs.getLong(14));
			hit.setFileName(rs.getString(15));
			hit.setStatus(rs.getInt(16));
			hit.setSigned(rs.getInt(17));
			hit.setType(rs.getString(18));
			hit.setRating(rs.getInt(19));
			hit.setFileVersion(rs.getString(20));
			hit.setComment(rs.getString(21));
			hit.setWorkflowStatus(rs.getString(22));
			hit.setStartPublishing(rs.getTimestamp(23));
			hit.setStopPublishing(rs.getTimestamp(24));
			hit.setPublished(rs.getInt(25));

			Folder folder = new Folder();
			folder.setName(rs.getString(26));
			folder.setId(rs.getLong(27));
			hit.setFolder(folder);

			if (rs.getLong(29) != 0L) {
				Template t = new Template();
				t.setId(rs.getLong(29));
				t.setName(rs.getString(30));
				hit.setTemplate(t);
				hit.setTemplateId(t.getId());
			}

			hit.setTenantId(rs.getLong(31));
			hit.setStamped(rs.getInt(33));
			hit.setPassword(rs.getString(34));
			hit.setWorkflowStatusDisplay(rs.getString(35));
			hit.setLanguage(rs.getString(36));
			hit.setPages(rs.getInt(37));
			hit.setColor(rs.getString(38));
			hit.setLastNote(rs.getString(39));
			hit.setRevision(rs.getString(40));
			
			return hit;
		}
	}

	protected FulltextSearch() {
	}

	@Override
	public void internalSearch() throws SearchException {
		FulltextSearchOptions opt = (FulltextSearchOptions) options;
		SearchEngine engine = Context.get(SearchEngine.class);

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
		Hits results = engine.search(query.toString(), filters, opt.getExpressionLanguage(), null);
		log.debug("End of Full-text search");
		log.debug("Fulltext hits count: {}", (results != null ? results.getCount() : 0));
		
		
		// Save here the binding between ID and Hit
		Map<Long, Hit> hitsMap = buildHitsMap(opt, results);

		if (hitsMap.isEmpty())
			return;

		estimatedHitsNumber = results != null ? results.getEstimatedCount() : 0;

		log.debug("DB search");

		Set<Long> hitsIds = hitsMap.keySet();
		StringBuilder hitsIdsCondition = new StringBuilder();
		if (!hitsIds.isEmpty()) {
			hitsIdsCondition.append(" and (");
			FolderDAO fdao = FolderDAO.get();
			if (fdao.isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
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
		richQuery.append(" A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable, ");
		richQuery.append(" A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type, ");
		richQuery.append(" A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing, ");
		richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
		richQuery.append(
				" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		richQuery.append(
				" A.ld_stamped, A.ld_password, A.ld_workflowstatusdisp, A.ld_language, A.ld_pages, A.ld_color, A.ld_lastnote, A.ld_revision ");
		richQuery.append(" from ld_document A ");
		richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
		richQuery.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");
		richQuery.append(" where A.ld_deleted=0 and not A.ld_status=" + DocumentStatus.ARCHIVED.ordinal()
				+ " and A.ld_nature=" + AbstractDocument.NATURE_DOC + " and A.ld_folderid=FOLD.ld_id  ");
		richQuery.append(" and A.ld_tenantid = " + tenantId);
		// For normal users we have to exclude not published documents
		if (searchUser != null && !searchUser.isMemberOf(Group.GROUP_ADMIN) && !searchUser.isMemberOf("publisher")) {
			richQuery.append(" and A.ld_published = 1 ");
			richQuery.append(" and A.ld_startpublishing <= CURRENT_TIMESTAMP ");
			richQuery.append(" and ( A.ld_stoppublishing is null or A.ld_stoppublishing > CURRENT_TIMESTAMP )");
		}
		richQuery.append("  and A.ld_docref is null ");
		richQuery.append(hitsIdsCondition.toString());

		if (options.isRetrieveAliases()) {
			// Append all aliases
			richQuery.append(
					" UNION select A.ld_id, REF.ld_customid, A.ld_docref, REF.ld_type, REF.ld_version, REF.ld_lastmodified, ");
			richQuery.append(
					" REF.ld_date, REF.ld_publisher, REF.ld_creation, REF.ld_creator, REF.ld_filesize, REF.ld_immutable, ");
			richQuery.append(
					" REF.ld_indexed, REF.ld_lockuserid, A.ld_filename, REF.ld_status, REF.ld_signed, REF.ld_type, ");
			richQuery.append(
					" REF.ld_rating, REF.ld_fileversion, A.ld_comment, REF.ld_workflowstatus, REF.ld_startpublishing, ");
			richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
			richQuery.append(
					" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, REF.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
			richQuery.append(
					" REF.ld_stamped, REF.ld_password, REF.ld_workflowstatusdisp, REF.ld_language, REF.ld_pages, A.ld_color, A.ld_lastnote, A.ld_revision ");
			richQuery.append(" from ld_document A  ");
			richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
			richQuery.append(" join ld_document REF on A.ld_docref=REF.ld_id ");
			richQuery.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");
			richQuery.append(" where A.ld_deleted=0 and not A.ld_status=" + DocumentStatus.ARCHIVED.ordinal()
					+ " and A.ld_nature=" + AbstractDocument.NATURE_DOC + " and A.ld_folderid=FOLD.ld_id ");
			richQuery.append(" and A.ld_tenantid = " + tenantId);
			// For normal users we have to exclude not published documents
			if (searchUser != null && !searchUser.isMemberOf(Group.GROUP_ADMIN)
					&& !searchUser.isMemberOf("publisher")) {
				richQuery.append(" and REF.ld_published = 1 ");
				richQuery.append(" and REF.ld_startpublishing <= CURRENT_TIMESTAMP ");
				richQuery.append(" and ( REF.ld_stoppublishing is null or REF.ld_stoppublishing > CURRENT_TIMESTAMP )");
			}
			richQuery.append("  and A.ld_docref is not null and REF.ld_deleted=0 and not A.ld_status="
					+ DocumentStatus.ARCHIVED.ordinal() + " and A.ld_docref = REF.ld_id ");
			richQuery.append(hitsIdsCondition.toString().replace("A.ld_id", "A.ld_docref"));
		}

		log.debug("Execute query {}", richQuery);

		DocumentDAO dao = DocumentDAO.get();
		try {
			dao.query(richQuery.toString(), new HitMapper(hitsMap), null);
		} catch (PersistenceException e) {
			throw new SearchException(e);
		}

		// Now sort the hits by score desc
		List<Hit> sortedHitsList = new ArrayList<>(hitsMap.values());
		Collections.sort(sortedHitsList);

		// Populate the hits list discarding unaccessible documents
		propulateHits(opt, accessibleFolderIds, sortedHitsList);
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

	private void propulateHits(FulltextSearchOptions opt, Collection<Long> accessibleFolderIds,
			List<Hit> sortedHitsList) throws SearchException {

		Set<Long> forbiddenDocs = getDeniedDocIds(sortedHitsList, accessibleFolderIds);

		Iterator<Hit> iter = sortedHitsList.iterator();
		while (iter.hasNext()) {
			if (options.getMaxHits() > 0 && hits.size() >= options.getMaxHits()) {
				// The maximum number of hits was reached
				moreHitsPresent = true;
				break;
			}
			Hit hit = iter.next();
			if (StringUtils.isNotEmpty(hit.getFileName())
					&& ((searchUser.isMemberOf(Group.GROUP_ADMIN) && opt.getFolderId() == null)
							|| (accessibleFolderIds != null && accessibleFolderIds.contains(hit.getFolder().getId())))
					&& !forbiddenDocs.contains(hit.getId()))
				hits.add(hit);
		}
	}

	private void setQueryFilters(FulltextSearchOptions opt, Set<String> filters, long tenantId,
			Collection<Long> accessibleFolderIds) throws SearchException, PersistenceException {
		if (searchUser != null && TenantDAO.get().count() > 1)
			filters.add(HitField.TENANT_ID + ":" + (tenantId < 0 ? "\\" : "") + tenantId);

		if (opt.getTemplate() != null)
			filters.add(HitField.TEMPLATE_ID + ":" + (opt.getTemplate() < 0 ? "\\" : "") + opt.getTemplate());

		if (StringUtils.isNotEmpty(opt.getLanguage()))
			filters.add(HitField.LANGUAGE + ":" + opt.getLanguage());

		if (opt.getSizeMin() != null)
			filters.add(HitField.SIZE + ":[" + opt.getSizeMin() + " TO *]");

		if (opt.getSizeMax() != null)
			filters.add(HitField.SIZE + COLON_STAR_TO + opt.getSizeMax() + "]");

		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		if (opt.getDateFrom() != null)
			filters.add(HitField.DATE + ":[" + df.format(opt.getDateFrom()) + "T00:00:00Z TO *]");

		if (opt.getDateTo() != null)
			filters.add(HitField.DATE + COLON_STAR_TO + df.format(opt.getDateTo()) + "T00:00:00Z]");

		if (opt.getCreationFrom() != null)
			filters.add(HitField.CREATION + ":[" + df.format(opt.getCreationFrom()) + "T00:00:00Z TO *]");

		if (opt.getCreationTo() != null)
			filters.add(HitField.CREATION + COLON_STAR_TO + df.format(opt.getCreationTo()) + "T00:00:00Z]");

		appendFolderQueryFilter(opt, filters, accessibleFolderIds);
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