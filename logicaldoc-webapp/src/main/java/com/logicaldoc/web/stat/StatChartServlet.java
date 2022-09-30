package com.logicaldoc.web.stat;

import java.awt.Color;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PiePlot;
import org.jfree.data.general.DefaultPieDataset;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.service.SystemServiceImpl;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet generates the pie charts displayed in the statistics.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class StatChartServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(StatChartServlet.class);

	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		File chartFile = null;

		try {
			Session session = ServletUtil.validateSession(request);

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			User user = session.getUser();

			// Create the folder for the chart
			chartFile = File.createTempFile("chart", ".png");

			String chart = request.getParameter("chart");
			SystemServiceImpl service = new SystemServiceImpl();
			GUIParameter[][] parameters = service.getStatistics(user.getLanguage());
			int index = 0;
			if ("repository".equals(chart))
				index = 0;
			else if ("documents".equals(chart))
				index = 1;
			else if ("pages".equals(chart))
				index = 2;
			if ("folders".equals(chart))
				index = 3;

			DefaultPieDataset dataset = new DefaultPieDataset();
			long total = 0;
			for (GUIParameter param : parameters[index]) {
				if (param == null)
					continue;
				total += Long.parseLong(param.getValue() != null ? param.getValue() : "0");
			}
			for (int i = 0; i < parameters[index].length; i++) {
				GUIParameter param = parameters[index][i];
				if (param != null) {
					double val = param.getValue() != null ? Long.parseLong(param.getValue()) : 0L;
					if (total > 0) {
						val = val * 100 / total;
						if (val >= 1)
							dataset.setValue(I18N.message(param.getName(), user.getLocale()), val);
					}
				}
			}

			Font font = null;
			try {
				GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
				if (ge != null) {
					String[] fontNames = ge.getAvailableFontFamilyNames();

					for (String name : fontNames)
						if ("arial".equalsIgnoreCase(name))
							font = new Font("Arial", Font.PLAIN, 12);
						else if ("helvetica".equalsIgnoreCase(name))
							font = new Font("Helvetica", Font.PLAIN, 12);
						else if ("Times New Roman".equalsIgnoreCase(name))
							font = new Font("Times New Roman", Font.PLAIN, 12);

					if (font == null && fontNames != null && fontNames.length > 0)
						font = new Font(fontNames[0], Font.PLAIN, 12);
				}
			} catch (Throwable t) {
				log.warn(t.getMessage());
			}

			JFreeChart chrt = ChartFactory.createPieChart(font != null ? I18N.message(chart, user.getLocale()) : null,
					dataset, font != null ? true : false, false, false);
			chrt.setBorderVisible(false);
			PiePlot plot = (PiePlot) chrt.getPlot();
			plot.setLabelGenerator(null);

			if (font == null) {
				log.warn("cannot find any usable font");
				chrt.setTitle((String) null);
			} else {
				chrt.getTitle().setPaint(new Color(110, 110, 110));
				chrt.getLegend().setBorder(0, 0, 0, 0);
				plot.setLabelFont(font);
				plot.setLabelGap(0.02);
			}

			plot.setNoDataMessage("No data available");
			plot.setCircular(true);
			plot.setBackgroundPaint(Color.WHITE);
			plot.setOutlinePaint(null);
			plot.setIgnoreNullValues(false);
			plot.setDrawingSupplier(new ChartDrawingSupplier());

			if (plot instanceof PiePlot) {
				PiePlot piePlot = (PiePlot) plot;
				piePlot.setInteriorGap(0.05);
			}

			ChartUtilities.saveChartAsPNG(chartFile, chrt, 250, 250);

			ServletUtil.downloadFile(request, response, chartFile, chart + ".png");
		} catch (Throwable ex) {
			log.error(ex.getMessage(), ex);
		} finally {
			FileUtil.strongDelete(chartFile);
		}
	}
}
