package com.logicaldoc.core.searchengine;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.ibm.icu.text.SimpleDateFormat;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;

/**
 * Search specialization for the Full text search.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.2
 */
public class FulltextSearch extends Search {

	public class HitMapper implements RowMapper<Hit> {

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
			hit.setIndexed(rs.getInt(13));
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

			return hit;
		}
	};

	protected FulltextSearch() {
	}

	@Override
	public void internalSearch() throws Exception {
		FulltextSearchOptions opt = (FulltextSearchOptions) options;
		SearchEngine engine = (SearchEngine) Context.get().getBean(SearchEngine.class);

		if (opt.getFields() == null) {
			String[] fields = new String[] { HitField.FILENAME.toString(), HitField.TAGS.toString(),
					HitField.CONTENT.toString() };
			opt.setFields(fields);
		}

		/*
		 * Prepare the query: the expression must be applied to all requested
		 * fields.
		 */
		StringBuffer query = new StringBuffer();
		for (String field : opt.getFields()) {
			if (query.length() > 0)
				query.append(" OR ");

			query.append(field + ":(" + opt.getExpression() + ")");
		}

		/*
		 * Prepare the filters
		 */
		ArrayList<String> filters = new ArrayList<String>();

		TenantDAO tdao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		long tenantId = Tenant.DEFAULT_ID;
		if (opt.getTenantId() != null)
			tenantId = opt.getTenantId().longValue();
		else if (searchUser != null)
			tenantId = searchUser.getTenantId();

		if (searchUser != null && tdao.count() > 1)
			filters.add(HitField.TENANT_ID + ":" + (tenantId < 0 ? "\\" : "") + tenantId);

		if (opt.getTemplate() != null)
			filters.add(HitField.TEMPLATE_ID + ":" + (opt.getTemplate() < 0 ? "\\" : "") + opt.getTemplate());

		if (StringUtils.isNotEmpty(opt.getLanguage()))
			filters.add(HitField.LANGUAGE + ":" + opt.getLanguage());

		if (opt.getSizeMin() != null)
			filters.add(HitField.SIZE + ":[" + opt.getSizeMin() + " TO *]");

		if (opt.getSizeMax() != null)
			filters.add(HitField.SIZE + ":[* TO " + opt.getSizeMax() + "]");

		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		if (opt.getDateFrom() != null)
			filters.add(HitField.DATE + ":[" + df.format(opt.getDateFrom()) + "T00:00:00Z TO *]");

		if (opt.getDateTo() != null)
			filters.add(HitField.DATE + ":[* TO " + df.format(opt.getDateTo()) + "T00:00:00Z]");

		if (opt.getCreationFrom() != null)
			filters.add(HitField.CREATION + ":[" + df.format(opt.getCreationFrom()) + "T00:00:00Z TO *]");

		if (opt.getCreationTo() != null)
			filters.add(HitField.CREATION + ":[* TO " + df.format(opt.getCreationTo()) + "T00:00:00Z]");

		/*
		 * We have to see what folders the user can access. But we need to
		 * perform this check only if the search is not restricted to one folder
		 * only.
		 */
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Collection<Long> accessibleFolderIds = new TreeSet<Long>();
		boolean searchInSingleFolder = (opt.getFolderId() != null && !opt.isSearchInSubPath());
		if (!searchInSingleFolder) {
			log.debug("Folders search");
			accessibleFolderIds = fdao.findFolderIdByUserId(opt.getUserId(), opt.getFolderId(), true);
			log.debug("End of Folders search");
		}
		if (opt.getFolderId() != null && !accessibleFolderIds.contains(opt.getFolderId())
				&& fdao.isReadEnabled(opt.getFolderId().longValue(), opt.getUserId()))
			accessibleFolderIds.add(opt.getFolderId());

		if (!accessibleFolderIds.isEmpty() && opt.getFolderId() != null) {
			StringBuffer folderFilter = new StringBuffer();
			for (Long id : accessibleFolderIds) {
				if (folderFilter.length() > 0)
					folderFilter.append(" or ");
				folderFilter.append(HitField.FOLDER_ID + ":" + (id < 0 ? "\\" : "") + id);
			}
			filters.add(folderFilter.toString());
		}

		/*
		 * Launch the search
		 */
		log.debug("Full-text seach: {}", query);
		Hits results = engine.search(query.toString(), filters.toArray(new String[0]), opt.getExpressionLanguage(),
				null);
		log.debug("End of Full-text search");
		log.debug("Fulltext hits count: {}", (results != null ? results.getCount() : 0));

		// Save here the binding between ID and Hit
		Map<Long, Hit> hitsMap = new HashMap<Long, Hit>();
		while (results != null && results.hasNext()) {
			Hit hit = results.next();

			// Skip a document if not in the filter set
			if (opt.getFilterIds() != null && !opt.getFilterIds().isEmpty()) {
				if (!opt.getFilterIds().contains(hit.getId()))
					continue;
			}
			hitsMap.put(hit.getId(), hit);
		}

		if (hitsMap.isEmpty())
			return;

		estimatedHitsNumber = results != null ? results.getEstimatedCount() : 0;

		log.debug("DB search");

		String hitsIdsStr = hitsMap.keySet().toString().replace('[', '(').replace(']', ')');

		StringBuffer richQuery = new StringBuffer();
		// Find real documents
		richQuery = new StringBuffer(
				"select A.ld_id, A.ld_customid, A.ld_docref, A.ld_type, A.ld_version, A.ld_lastmodified, ");
		richQuery.append(" A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable, ");
		richQuery.append(" A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type, ");
		richQuery.append(" A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing, ");
		richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
		richQuery
				.append(" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		richQuery.append(" A.ld_stamped, A.ld_password ");
		richQuery.append(" from ld_document A ");
		richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
		richQuery.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");
		richQuery.append(" where A.ld_deleted=0 and A.ld_nature=" + AbstractDocument.NATURE_DOC
				+ " and A.ld_folderid=FOLD.ld_id  ");
		richQuery.append(" and A.ld_tenantid = " + tenantId);
		// For normal users we have to exclude not published documents
		if (searchUser != null && !searchUser.isMemberOf("admin") && !searchUser.isMemberOf("publisher")) {
			richQuery.append(" and A.ld_published = 1 ");
			richQuery.append(" and A.ld_startpublishing <= CURRENT_TIMESTAMP ");
			richQuery.append(" and ( A.ld_stoppublishing is null or A.ld_stoppublishing > CURRENT_TIMESTAMP )");
		}
		richQuery.append("  and A.ld_docref is null ");
		richQuery.append("  and A.ld_id in ");
		richQuery.append(hitsIdsStr);

		// Append all aliases
		richQuery
				.append(" UNION select A.ld_id, REF.ld_customid, A.ld_docref, REF.ld_type, REF.ld_version, REF.ld_lastmodified, ");
		richQuery
				.append(" REF.ld_date, REF.ld_publisher, REF.ld_creation, REF.ld_creator, REF.ld_filesize, REF.ld_immutable, ");
		richQuery
				.append(" REF.ld_indexed, REF.ld_lockuserid, REF.ld_filename, REF.ld_status, REF.ld_signed, REF.ld_type, ");
		richQuery
				.append(" REF.ld_rating, REF.ld_fileversion, A.ld_comment, REF.ld_workflowstatus, REF.ld_startpublishing, ");
		richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
		richQuery
				.append(" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, REF.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		richQuery.append(" REF.ld_stamped, REF.ld_password ");
		richQuery.append(" from ld_document A  ");
		richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
		richQuery.append(" join ld_document REF on A.ld_docref=REF.ld_id ");
		richQuery.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");
		richQuery.append(" where A.ld_deleted=0 and A.ld_nature=" + AbstractDocument.NATURE_DOC
				+ " and A.ld_folderid=FOLD.ld_id ");
		richQuery.append(" and A.ld_tenantid = " + tenantId);
		// For normal users we have to exclude not published documents
		if (searchUser != null && !searchUser.isMemberOf("admin") && !searchUser.isMemberOf("publisher")) {
			richQuery.append(" and REF.ld_published = 1 ");
			richQuery.append(" and REF.ld_startpublishing <= CURRENT_TIMESTAMP ");
			richQuery.append(" and ( REF.ld_stoppublishing is null or REF.ld_stoppublishing > CURRENT_TIMESTAMP )");
		}
		richQuery.append("  and A.ld_docref is not null and REF.ld_deleted=0 and A.ld_docref = REF.ld_id ");
		richQuery.append("  and A.ld_id in ");
		richQuery.append(hitsIdsStr);

		log.debug("Execute query\n" + richQuery.toString());

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		dao.query(richQuery.toString(), null, new HitMapper(hitsMap), null);

		// Now sort the hits by score desc
		List<Hit> sortedHitsList = new ArrayList<Hit>(hitsMap.values());
		try {
			Collections.sort(sortedHitsList);
		} catch (Throwable t) {
			log.warn(t.getMessage());
		}

		// Populate the hits list discarding unexisting documents
		Iterator<Hit> iter = sortedHitsList.iterator();
		while (iter.hasNext()) {
			if (options.getMaxHits() > 0 && hits.size() >= options.getMaxHits()) {
				// The maximum number of hits was reached
				moreHitsPresent = true;
				break;
			}
			Hit hit = iter.next();

			if (StringUtils.isEmpty(hit.getFileName()))
				continue;

			if ((searchUser.isMemberOf("admin") && opt.getFolderId() == null)
					|| (accessibleFolderIds != null && accessibleFolderIds.contains(hit.getFolder().getId())))
				hits.add(hit);
		}

		/*
		 * Check for suggestions
		 */
		if (results != null) {
			Map<String, String> suggestions = (Map<String, String>) results.getSuggestions();
			if (!results.getSuggestions().isEmpty()) {
				suggestion = options.getExpression();
				for (String token : results.getSuggestions().keySet())
					suggestion = suggestion.replaceFirst(token, suggestions.get(token));
			}
		}
	}
}