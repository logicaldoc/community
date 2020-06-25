package com.logicaldoc.gui.frontend.client.reports.custom;

import java.util.LinkedHashMap;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewTile;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;

/**
 * Shows report's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class ReportStandardProperties extends ReportDetailsTab {
	private DynamicForm form = new DynamicForm();

	private HLayout columns = new HLayout();

	private FolderSelector outputFolderSelector;

	private Layout tile = new HLayout();

	private GUIReport report;

	public ReportStandardProperties(GUIReport report, final ChangedHandler changedHandler) {
		super(report, changedHandler);
		setWidth100();
		setHeight100();

		this.report = report;

		setMembers(columns);
		outputFolderSelector = new FolderSelector("outputFolder", false);
		outputFolderSelector.setRequired(true);
		outputFolderSelector.setWidth(250);
		outputFolderSelector.setTitle(I18N.message("outputfolder"));
		if (report.getOutputFolder() != null)
			outputFolderSelector.setFolder(report.getOutputFolder());
		outputFolderSelector.addFolderChangeListener(new FolderChangeListener() {
			@Override
			public void onChanged(GUIFolder folder) {
				changedHandler.onChanged(null);
			}
		});

		refresh();
	}

	private void refresh() {
		if (form != null)
			form.destroy();

		if (tile != null)
			tile.destroy();

		if (columns.contains(form))
			columns.removeChild(form);

		if (columns.contains(tile))
			columns.removeChild(tile);

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", "id", Long.toString(report.getId()));

		StaticTextItem name = ItemFactory.newStaticTextItem("name", "name", report.getName());

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description", report.getDescription());
		description.setWidth(250);
		description.addChangedHandler(changedHandler);

		SelectItem format = ItemFactory.newSelectItem("outputFormat", "format");
		format.addChangedHandler(changedHandler);
		format.setRequired(true);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("pdf", "pdf");
		map.put("xlsx", "xlsx");
		map.put("html", "html");
		format.setValueMap(map);
		format.setValue(report.getOutputFormat());

		SelectItem updatePolicy = ItemFactory.newSelectItem("updatePolicy", "onupdate");
		updatePolicy.addChangedHandler(changedHandler);
		LinkedHashMap<String, String> map2 = new LinkedHashMap<String, String>();
		map2.put("0", I18N.message("createnewversion"));
		map2.put("1", I18N.message("createnewdoc"));
		updatePolicy.setValueMap(map2);
		updatePolicy.setValue(Integer.toString(report.getUpdatePolicy()));

		String downloadUrl = "";
		if (report.getOutputDocId() != null)
			downloadUrl = Util.downloadURL(report.getOutputDocId());
		String perma = "<a href='" + downloadUrl + "'>" + I18N.message("download") + "</a>";
		StaticTextItem outputLink = ItemFactory.newStaticTextItem("output", "output", perma);

		if (report.getOutputDocId() != null)
			form.setItems(id, outputLink, name, outputFolderSelector, format, updatePolicy, description);
		else
			form.setItems(id, name, outputFolderSelector, format, updatePolicy, description);

		columns.addMember(form);

		/*
		 * Prepare the tile
		 */
		if (report.getOutputDocId() != null) {
			tile = new PreviewTile(report.getOutputDocId(), report.getName());
			columns.addMember(tile);
		}
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) form.getValues();
		form.validate();
		if (!form.hasErrors()) {
			report.setName((String) values.get("name"));
			report.setDescription((String) values.get("description"));
			report.setOutputFormat((String) values.get("outputFormat"));
			report.setUpdatePolicy(Integer.parseInt(values.get("updatePolicy").toString()));
			report.setOutputFolder(outputFolderSelector.getFolder());
		}
		return !form.hasErrors();
	}
}