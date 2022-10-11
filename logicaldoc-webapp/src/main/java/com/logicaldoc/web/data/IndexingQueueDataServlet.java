package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.IndexerTask;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for retrieving the current indexing queue
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
public class IndexingQueueDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale) throws PersistenceException, IOException {

		UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = dao.findById(session.getUserId());
		dao.initialize(user);

		String where = prepareWhere(request, session);

		StringBuffer query = new StringBuffer(
				"select ld_id, ld_customid, ld_docref, ld_type, ld_version, ld_lastModified, ld_date, ld_publisher,"
						+ " ld_creation, ld_creator, ld_filesize, ld_immutable, ld_indexed, ld_lockuserid, ld_filename, ld_status,"
						+ " ld_signed, ld_type, ld_fileversion, ld_color from ld_document where " + where);

		/*
		 * Execute the Query
		 */
		@SuppressWarnings("unchecked")
		List<Object[]> records = (List<Object[]>) dao.query(query.toString(), null, new RowMapper<Object[]>() {

			@Override
			public Object[] mapRow(ResultSet rs, int row) throws SQLException {
				Object[] rec = new Object[20];
				rec[0] = rs.getLong(1);
				rec[1] = rs.getString(2);
				rec[2] = rs.getLong(3) != 0 ? rs.getLong(3) : null;
				rec[3] = rs.getString(4);
				// version
				rec[4] = rs.getString(5);
				rec[5] = rs.getTimestamp(6);
				rec[6] = rs.getTimestamp(7);
				rec[7] = rs.getString(8);
				rec[8] = rs.getTimestamp(9);
				// creator
				rec[9] = rs.getString(10);
				rec[10] = rs.getLong(11);
				rec[11] = rs.getInt(12);
				rec[12] = rs.getInt(13);
				rec[13] = rs.getLong(14);
				// filename
				rec[14] = rs.getString(15);
				rec[15] = rs.getInt(16);
				// signed
				rec[16] = rs.getInt(17);
				// type
				rec[17] = rs.getString(18);
				// fileversion
				rec[18] = rs.getString(19);
				// color
				rec[19] = rs.getString(20);
				return rec;
			}
		}, max);

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Iterate over records composing the response XML document
		 */
		ContextProperties config = Context.get().getProperties();
		for (Object[] cols : records) {
			if (!FileUtil.matches(cols[14].toString(),
					config.getProperty(session.getTenantName() + ".barcode.includes"),
					config.getProperty(session.getTenantName() + ".barcode.excludes")))
				continue;

			writer.print("<document>");
			writer.print("<id>" + cols[0] + "</id>");
			if (cols[1] != null)
				writer.print("<customId><![CDATA[" + cols[1] + "]]></customId>");
			else
				writer.print("<customId> </customId>");
			writer.print("<docref>" + (cols[2] != null ? cols[2] : "") + "</docref>");
			writer.print("<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon((String) cols[3])) + "</icon>");

			writer.print("<version>" + cols[4] + "</version>");
			writer.print("<lastModified>" + df.format(cols[5]) + "</lastModified>");
			writer.print("<published>" + df.format(cols[6]) + "</published>");
			writer.print("<publisher><![CDATA[" + cols[7] + "]]></publisher>");
			writer.print("<created>" + df.format(cols[8]) + "</created>");
			writer.print("<creator><![CDATA[" + cols[9] + "]]></creator>");
			writer.print("<size>" + cols[10] + "</size>");
			if (Integer.parseInt(cols[11].toString()) == 0)
				writer.print("<immutable>blank</immutable>");
			else if (Integer.parseInt(cols[11].toString()) == 1)
				writer.print("<immutable>stop</immutable>");
			if (Integer.parseInt(cols[12].toString()) == Constants.INDEX_TO_INDEX)
				writer.print("<indexed>blank</indexed>");
			else if (Integer.parseInt(cols[12].toString()) == Constants.INDEX_INDEXED)
				writer.print("<indexed>indexed</indexed>");
			else if (Integer.parseInt(cols[12].toString()) == Constants.INDEX_SKIP)
				writer.print("<indexed>unindexable</indexed>");
			if (Integer.parseInt(cols[15].toString()) == Constants.DOC_LOCKED)
				writer.print("<locked>lock</locked>");
			else if (Integer.parseInt(cols[15].toString()) == Constants.DOC_CHECKED_OUT)
				writer.print("<locked>page_edit</locked>");
			else
				writer.print("<locked>blank</locked>");
			if (cols[14] != null)
				writer.print("<lockUserId>" + cols[13] + "</lockUserId>");
			writer.print("<filename><![CDATA[" + cols[14] + "]]></filename>");
			writer.print("<status>" + cols[15] + "</status>");

			if (Integer.parseInt(cols[16].toString()) == 0)
				writer.print("<signed>blank</signed>");
			else if (Integer.parseInt(cols[16].toString()) == 1)
				writer.print("<signed>rosette</signed>");

			writer.print("<type>" + cols[17] + "</type>");

			writer.print("<fileVersion><![CDATA[" + cols[18] + "]]></fileVersion>");

			if (cols[19] != null)
				writer.print("<color><![CDATA[" + cols[19] + "]]></color>");

			writer.print("</document>");
		}
		writer.write("</list>");
	}

	/**
	 * Prepares the where clause to retrieve the documents
	 * 
	 * @param request the current servlet request
	 * @param session the current session
	 * 
	 * @return the where clause to use in the documents query composition
	 */
	protected String prepareWhere(HttpServletRequest request, Session session) {
		/**
		 * Prepare the query, note that the fragments are in HQL so we convert
		 * them into plain SQL.
		 */
		String[] queryFragments = IndexerTask.prepareQuery();

		StringBuffer where = new StringBuffer(queryFragments[0].replaceAll("_entity.", "ld_"));

		where.append((StringUtils.isNotEmpty(queryFragments[1])
				? " order by " + queryFragments[1].replaceAll("_entity.", "ld_")
				: ""));

		return where.toString();
	}
}