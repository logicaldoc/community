package com.logicaldoc.core.searchengine;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;

/**
 * Search specialization for the Tag search.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class TagSearch extends Search {

	protected TagSearch() {
	}

	@SuppressWarnings("unchecked")
	@Override
	public void internalSearch() throws SearchException {
		prepareExpression();

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			hits.addAll((List<Hit>) dao.query(options.getExpression(), null, new HitMapper(), options.getMaxHits()));
		} catch (PersistenceException e) {
			throw new SearchException(e);
		}

		moreHitsPresent = (hits.size() >= options.getMaxHits());
		if (moreHitsPresent)
			estimatedHitsNumber = hits.size() + 1;
		else
			estimatedHitsNumber = hits.size();
	}

	/**
	 * Utility method that prepare the query expression.
	 */
	private void prepareExpression() {

		// Find all real documents
		StringBuffer query = new StringBuffer(
				"select A.ld_id, A.ld_customid, A.ld_docref, A.ld_type, A.ld_version, A.ld_lastmodified, ");
		query.append(" A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable, ");
		query.append(" A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type, ");
		query.append(" A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing, ");
		query.append(" A.ld_stoppublishing, A.ld_published, ");
		query.append(" B.ld_name, A.ld_folderid, A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		query.append(" A.ld_stamped, A.ld_password, A.ld_workflowstatusdisp, A.ld_language ");
		query.append(" from ld_document A ");
		query.append(" join ld_folder B on A.ld_folderid=B.ld_id ");
		query.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");

		appendWhereClause(false, query);

		// Append all shortcuts
		query.append(
				" UNION select A.ld_id, REF.ld_customid, A.ld_docref, REF.ld_type, REF.ld_version, REF.ld_lastmodified, ");
		query.append(
				" REF.ld_date, REF.ld_publisher, REF.ld_creation, REF.ld_creator, REF.ld_filesize, REF.ld_immutable, ");
		query.append(
				" REF.ld_indexed, REF.ld_lockuserid, REF.ld_filename, REF.ld_status, REF.ld_signed, REF.ld_type, ");
		query.append(
				" REF.ld_rating, REF.ld_fileversion, REF.ld_comment, REF.ld_workflowstatus, A.ld_startpublishing, ");
		query.append(" A.ld_stoppublishing, A.ld_published, ");
		query.append(" B.ld_name, A.ld_folderid, REF.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		query.append(" REF.ld_stamped, REF.ld_password, REF.ld_workflowstatusdisp, REF.ld_language ");
		query.append(" from ld_document A ");
		query.append(" join ld_folder B on A.ld_folderid=B.ld_id ");
		query.append(" join ld_document REF on A.ld_docref=REF.ld_id ");
		query.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");

		appendWhereClause(true, query);

		log.info("executing tag search query={}", query.toString());

		options.setExpression(query.toString());
	}

	/**
	 * This method appends the where clause considering or not the shortcut on
	 * the search.
	 * 
	 * @param aliases If true, also the shortcut must be considered in the
	 *        search
	 * @param query
	 */
	private void appendWhereClause(boolean aliases, StringBuffer query) {
		long tenantId = Tenant.DEFAULT_ID;
		if (options.getTenantId() != null)
			tenantId = options.getTenantId().longValue();
		else if (searchUser != null)
			tenantId = searchUser.getTenantId();

		query.append(" where A.ld_deleted=0 and A.ld_nature=" + AbstractDocument.NATURE_DOC
				+ " and A.ld_folderid=B.ld_id and A.ld_tenantid = ");
		query.append(tenantId);
		query.append(" and not A.ld_status = " + AbstractDocument.DOC_ARCHIVED);

		// Ids string to be used in the query
		String ids = null;
		if (getOptions().getFilterIds() != null && !getOptions().getFilterIds().isEmpty()) {
			ids = getOptions().getFilterIds().toString();
			ids = ids.replace('[', '(').replace(']', ')');
		}

		if (StringUtils.isNotEmpty(ids) && !"()".equals(ids)) {
			query.append(" and A.ld_id in ");
			query.append(ids);
		}

		/*
		 * Now get the IDs of the documents tagged with searched tag and use
		 * them as filter
		 */
		if (aliases)
			query.append(
					" and A.ld_docref is not null and REF.ld_deleted=0 and A.ld_docref = REF.ld_id and A.ld_docref in ");
		else
			query.append(" and A.ld_docref is null and A.ld_id in ");
		DocumentDAO docDAO = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Long> precoll = docDAO.findDocIdByUserIdAndTag(options.getUserId(), options.getExpression());
		String buf = precoll.toString().replace("[", "(").replace("]", ")");
		query.append(!"()".equals(buf) ? buf : "(0)");

		// For normal users we have to exclude not published documents
		if (!searchUser.isMemberOf("admin") && !searchUser.isMemberOf("publisher")) {
			query.append(" and A.ld_published = 1 ");
			query.append(" and A.ld_startpublishing <= CURRENT_TIMESTAMP ");
			query.append(" and ( A.ld_stoppublishing is null or A.ld_stoppublishing > CURRENT_TIMESTAMP )");
		}
	}

	public class HitMapper implements RowMapper<Hit> {

		public Hit mapRow(ResultSet rs, int rowNum) throws SQLException {
			Hit hit = new Hit();
			hit.setId(rs.getLong(1));
			hit.setCustomId(rs.getString(2));
			hit.setDocRef(rs.getLong(3));
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

			if (rs.getLong(28) != 0L) {
				Template t = new Template();
				t.setId(rs.getLong(28));
				t.setName(rs.getString(29));
				hit.setTemplate(t);
				hit.setTemplateId(t.getId());
			}

			hit.setTenantId(rs.getLong(30));
			hit.setDocRefType(rs.getString(31));
			hit.setStamped(rs.getInt(32));
			hit.setPassword(rs.getString(33));
			hit.setWorkflowStatusDisplay(rs.getString(34));
			hit.setLanguage(rs.getString(35));

			return hit;
		}
	};
}