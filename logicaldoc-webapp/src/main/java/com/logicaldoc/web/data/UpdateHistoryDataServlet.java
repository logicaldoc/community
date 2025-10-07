package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.ResultSetWalker;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet collects all applied updates and patches
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 9.2.2
 */
public class UpdateHistoryDataServlet extends AbstractDataServlet {
	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {
		PrintWriter writer = response.getWriter();
		writer.print("<list>");

		DateFormat df = getDateFormat();

		UserDAO userDao = Context.get(UserDAO.class);
		userDao.queryForResultSet("""
select lower(ld_update), ld_date, ld_version, 0, 'update' from ld_update
 UNION
select lower(ld_patch), ld_date, ld_version, ld_rating, 'patch' from ld_patch 
""", null, null, new ResultSetWalker() {

			@Override
			public void walk(ResultSet rows) throws SQLException {
				while (rows.next()) {
					writer.print("<update>");

					String name = StringUtils.defaultString(rows.getString(1)).replace(".zip", "");
					writer.print("<name>" + name + "</name>");
					writer.print("<date>" + df.format(rows.getTimestamp(2)) + "</date>");
					writer.print("<version>" + rows.getString(3) + "</version>");
					writer.print("<rating>" + rows.getInt(4) + "</rating>");
					writer.print("<type>" + rows.getString(5) + "</type>");
					writer.print("</update>");
				}
			}
		});

		writer.print("</list>");
	}
}