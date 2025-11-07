package com.logicaldoc.web;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.ui.HorizontalAlignment;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.util.ServletUtil;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Base for those servlets that produce a graph to display some kind of
 * statistics.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public abstract class ChartServlet extends HttpServlet {

	private static final long serialVersionUID = 1;

	private static final Logger log = LoggerFactory.getLogger(ChartServlet.class);

	protected String sequencePrefix() {
		return "prefix";
	}

	protected String messageTitle() {
		return "key";
	}

	/**
	 * The doGet method of the servlet.
	 * 
	 * This method is called when a form has its tag value method equals to get.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 * 
	 * @throws ServletException if an error occurred
	 * @throws IOException if an error occurred
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		File chartFile = null;
		try {
			ServletUtil.validateSession(request);

			int width = 1200;
			if (request.getParameter("width") != null)
				width = Integer.parseInt(request.getParameter("width"));

			int height = 450;
			if (request.getParameter("height") != null)
				height = Integer.parseInt(request.getParameter("height"));

			Locale locale = Locale.ENGLISH;
			if (request.getParameter("locale") != null)
				locale = LocaleUtil.toLocale(request.getParameter("locale"));

			chartFile = FileUtil.createTempFile("chart", ".png");

			drawChart(chartFile, width, height, locale, getObjectId(request), getTenantId(request));

			ServletUtil.downloadFile(request, response, chartFile, "chart.png");
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		} finally {
			FileUtil.delete(chartFile);
		}
	}

	protected long getObjectId(HttpServletRequest request) throws IOException, PersistenceException {
		return Long.parseLong(StringUtils.defaultString(request.getParameter("objectId"), "0"));
	}

	protected long getTenantId(HttpServletRequest request) throws IOException, PersistenceException {
		return Long.parseLong(
				StringUtils.defaultString(request.getParameter("tenantId"), String.valueOf(Tenant.SYSTEM_ID)));
	}

	protected void drawChart(File chartFile, int width, int height, Locale locale, long objectId, long tenantId)
			throws IOException {
		/**
		 * Retrieve the sequences and order them by date
		 */
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();

		DateFormat dfName = new SimpleDateFormat("MMM yyyy", locale);
		DateFormat dfNumber = new SimpleDateFormat("yyyyMM");
		for (int i = 0; i < 36; i++) {
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.MONTH, -i);
			String month = dfNumber.format(cal.getTime());
			String monthName = dfName.format(cal.getTime());
			dataset.addValue(SequenceDAO.get().getCurrentValue(sequencePrefix() + "-" + month, objectId, tenantId),
					I18N.message(messageTitle(), locale), monthName);
		}

		JFreeChart chart = ChartFactory.createBarChart("", null, null, dataset, PlotOrientation.VERTICAL, false, false,
				false);

		CategoryPlot catPlot = chart.getCategoryPlot();
		catPlot.getRangeAxis().setStandardTickUnits(NumberAxis.createIntegerTickUnits());
		BarRenderer r = (BarRenderer) catPlot.getRenderer();
		r.setSeriesPaint(0, Color.BLUE);

		chart.getTitle().setHorizontalAlignment(HorizontalAlignment.LEFT);
		chart.getTitle().setPaint(Color.BLUE);

		CategoryAxis domainAxis = catPlot.getDomainAxis();
		domainAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_45);

		ChartUtilities.saveChartAsPNG(chartFile, chart, width, height);
	}
}