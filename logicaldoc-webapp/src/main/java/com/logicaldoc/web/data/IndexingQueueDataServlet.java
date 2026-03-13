package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.indexer.IndexerTask;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for retrieving the current indexing queue
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
public class IndexingQueueDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(IndexingQueueDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		UserDAO dao = UserDAO.get();
		User user = dao.findById(session.getUserId());
		dao.initialize(user);

		String query = """
                       select ld_id, ld_customid, ld_docref, ld_type, ld_version, ld_lastModified, ld_date, ld_publisher,
                              ld_creation, ld_creator, ld_filesize, ld_immutable, ld_indexed, ld_lockuserid, ld_filename, ld_status,
                              ld_signed, ld_type, ld_fileversion, ld_color 
                         from ld_document 
                        where %s
                        """.formatted(prepareWhere(request, session));

		if (log.isDebugEnabled())
			log.debug("Running filter query {}", query);

		/*
		 * Execute the Query.
		 */
		List<Object[]> records = dao.query(query, new RowMapper<Object[]>() {

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
		}, max != null ? max : 100);

		if (log.isDebugEnabled())
			log.debug("Retrieved {} records", records.size());

		printDocuments(response, session, records);
	}

	private void printDocuments(HttpServletResponse response, Session session, List<Object[]> records)
			throws IOException {
		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Iterate over records composing the response XML document
		 */
		ContextProperties config = Context.get().getConfig();
		for (Object[] cols : records) {
			if (!FileUtil.matches(cols[14].toString(),
					config.getProperty("%s.barcode.includes".formatted(session.getTenantName())),
					config.getProperty("%s.barcode.excludes".formatted(session.getTenantName()))))
				continue;

			printDocument(writer, cols);
		}
		writer.write("</list>");
	}

	private void printDocument(PrintWriter writer, Object[] cols) {
		DateFormat df = getDateFormat();

		writer.print("<document>");
		writer.print(String.format("<id>%d</id>", (Long) cols[0]));
		writer.print(String.format("<customId><![CDATA[%s]]></customId>", StringUtils.defaultString((String) cols[1])));

		writer.print(String.format("<docref>%s</docref>", StringUtils.defaultString((String) cols[2])));
		writer.print(String.format("<icon>%s</icon>", FileUtil.getBaseName(IconSelector.selectIcon((String) cols[3]))));

		writer.print(String.format("<version>%s</version>", cols[4]));
		writer.print(String.format("<lastModified>%s</lastModified>", df.format(cols[5])));
		writer.print(String.format("<published>%s</published>", df.format(cols[6])));
		writer.print(String.format("<publisher><![CDATA[%s]]></publisher>", cols[7]));
		writer.print(String.format("<created>%s</created>", df.format(cols[8])));
		writer.print(String.format("<creator><![CDATA[%s]]></creator>", cols[9]));
		writer.print(String.format("<size>%d</size>", (Long) cols[10]));

		if (Integer.parseInt(cols[11].toString()) == 0)
			writer.print("<immutable>blank</immutable>");
		else if (Integer.parseInt(cols[11].toString()) == 1)
			writer.print("<immutable>stop</immutable>");

		writer.print(String.format("<indexed>%d</indexed>", (Integer) cols[12]));

		if (Integer.parseInt(cols[15].toString()) == Constants.DOC_LOCKED)
			writer.print("<locked>lock</locked>");
		else if (Integer.parseInt(cols[15].toString()) == Constants.DOC_CHECKED_OUT)
			writer.print("<locked>page_edit</locked>");
		else
			writer.print("<locked>blank</locked>");

		if (cols[14] != null)
			writer.print(String.format("<lockUserId>%d</lockUserId>", (Long) cols[13]));
		writer.print(String.format("<filename><![CDATA[%s]]></filename>", cols[14]));
		writer.print(String.format("<status>%d</status>", (Integer) cols[15]));

		if (Integer.parseInt(cols[16].toString()) == 0)
			writer.print("<signed>blank</signed>");
		else if (Integer.parseInt(cols[16].toString()) == 1)
			writer.print("<signed>rosette</signed>");

		writer.print(String.format("<type>%s</type>", cols[17]));

		writer.print(String.format("<fileVersion><![CDATA[%s]]></fileVersion>", cols[18]));

		if (cols[19] != null)
			writer.print(String.format("<color><![CDATA[%s]]></color>", cols[19]));

		writer.print("</document>");
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

		StringBuilder where = new StringBuilder(queryFragments[0].replace("_entity.", "ld_"));

		where.append((StringUtils.isNotEmpty(queryFragments[1])
				? " order by %s".formatted(queryFragments[1].replace("_entity.", "ld_"))
				: ""));

		return where.toString().replace("ld_indexingStatus", "ld_indexed");
	}
}