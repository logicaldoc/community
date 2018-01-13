package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.NumberFormat;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Show differences between two versions at metadata level
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class MetadataDiff extends Window {

	public MetadataDiff(final GUIVersion version1, final GUIVersion version2) {
		super();

		setTitle(I18N.message("compare") + " " + version1.getVersion() + " - " + version2.getVersion());
		setWidth(450);
		setHeight(350);
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);
		setCanDragReposition(true);
		setCanDragResize(true);
		centerInPage();

		// Prepare the records, each one is related to a version's attribute
		ArrayList<DiffRecord> records = new ArrayList<DiffRecord>();
		records.add(new DiffRecord(I18N.message("versiondate"), I18N.formatDate(version1.getVersionDate()), I18N
				.formatDate(version2.getVersionDate())));
		records.add(new DiffRecord(I18N.message("fileversion"), version1.getFileVersion(), version2.getFileVersion()));
		records.add(new DiffRecord(I18N.message("filename"), version1.getFileName(), version2.getFileName()));
		records.add(new DiffRecord(I18N.message("username"), version1.getUsername(), version2.getUsername()));
		records.add(new DiffRecord(I18N.message("comment"), version1.getComment(), version2.getComment()));
		records.add(new DiffRecord(I18N.message("customid"), version1.getCustomId(), version2.getCustomId()));
		records.add(new DiffRecord(I18N.message("language"), version1.getLanguage(), version2.getLanguage()));
		records.add(new DiffRecord(I18N.message("createdon"), I18N.formatDate(version1.getCreation()), I18N
				.formatDate(version2.getCreation())));
		records.add(new DiffRecord(I18N.message("creator"), version1.getCreator(), version2.getCreator()));
		records.add(new DiffRecord(I18N.message("publishedon"), I18N.formatDate(version1.getDate()), I18N
				.formatDate(version2.getDate())));
		records.add(new DiffRecord(I18N.message("publisher"), version1.getPublisher(), version2.getPublisher()));
		records.add(new DiffRecord(I18N.message("size"), Util.formatSizeKB(version1.getFileSize()), Util
				.formatSizeKB(version2.getFileSize())));
		records.add(new DiffRecord(I18N.message("folder"),
				version1.getFolder().getId() == Constants.DOCUMENTS_FOLDERID ? "/" : version1.getFolder().getName(),
				version2.getFolder().getId() == Constants.DOCUMENTS_FOLDERID ? "/" : version2.getFolder().getName()));

		records.add(new DiffRecord(I18N.message("tags"), version1.getTagsString(), version2.getTagsString()));

		records.add(new DiffRecord(I18N.message("published"), version1.getPublished() == 1 ? I18N.message("yes") : I18N
				.message("no"), version2.getPublished() == 1 ? I18N.message("yes") : I18N.message("no")));

		records.add(new DiffRecord(I18N.message("startpublishing"), version1.getStartPublishing() != null ? I18N
				.formatDate(version1.getStartPublishing()) : null, version2.getStartPublishing() != null ? I18N
				.formatDate(version2.getStartPublishing()) : null));

		records.add(new DiffRecord(I18N.message("stoppublishing"), version1.getStopPublishing() != null ? I18N
				.formatDate(version1.getStopPublishing()) : null, version2.getStopPublishing() != null ? I18N
				.formatDate(version2.getStopPublishing()) : null));

		records.add(new DiffRecord(I18N.message("template"), version1.getTemplate(), version2.getTemplate()));
		printExtendedAttributes(records, version1, version2);

		ListGridField name = new ListGridField("name", " ");
		ListGridField val1 = new ListGridField("val1", version1.getVersion());
		ListGridField val2 = new ListGridField("val2", version2.getVersion());

		ListGrid listGrid = new ListGrid();
		listGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		listGrid.setCanFreezeFields(false);
		listGrid.setCanGroupBy(false);
		listGrid.setAutoFetchData(true);
		listGrid.setCanReorderFields(false);
		listGrid.setCanSort(false);
		listGrid.setData(records.toArray(new ListGridRecord[0]));
		listGrid.setFields(name, val1, val2);
		addItem(listGrid);

		IButton compareContent = new IButton(I18N.message("comparecontent"));
		compareContent.setWidth100();
		compareContent.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				ContentDiff diff = new ContentDiff(version1.getDocId(), version1.getFileVersion(), version2
						.getFileVersion());
				diff.show();
			}
		});
		if (Feature.visible(Feature.CONTENT_DIFF)) {
			addItem(compareContent);
			compareContent.setDisabled(!Feature.enabled(Feature.CONTENT_DIFF));
		}
	}

	private void printExtendedAttributes(ArrayList<DiffRecord> records, GUIVersion version1, GUIVersion version2) {
		DateTimeFormat dateFormat = DateTimeFormat.getFormat(I18N.message("format_date"));
		NumberFormat numberFormat = NumberFormat.getDecimalFormat();

		List<String> names = new ArrayList<String>();

		// Collect all attribute names from version1
		for (GUIAttribute att : version1.getAttributes()) {
			if (!names.contains(att.getName()))
				names.add(att.getName());
		}
		// Collect all attribute names from version2
		for (GUIAttribute att : version2.getAttributes()) {
			if (!names.contains(att.getName()))
				names.add(att.getName());
		}

		for (String name : names) {
			GUIAttribute att = version1.getAttribute(name);
			String val1 = "";
			if (att != null)
				if ((att.getType() == GUIAttribute.TYPE_STRING || att.getType() == GUIAttribute.TYPE_USER)
						&& att.getStringValue() != null) {
					val1 = att.getStringValue();
				} else if (att.getType() == GUIAttribute.TYPE_INT && att.getValue() != null) {
					val1 = Long.toString(att.getIntValue());
				} else if (att.getType() == GUIAttribute.TYPE_DOUBLE && att.getValue() != null) {
					val1 = numberFormat.format(att.getDoubleValue());
				} else if (att.getType() == GUIAttribute.TYPE_DATE && att.getValue() != null) {
					val1 = dateFormat.format(att.getDateValue());
				}

			att = version2.getAttribute(name);
			String val2 = "";
			if (att != null)
				if ((att.getType() == GUIAttribute.TYPE_STRING || att.getType() == GUIAttribute.TYPE_USER)
						&& att.getStringValue() != null) {
					val2 = att.getStringValue();
				} else if (att.getType() == GUIAttribute.TYPE_INT && att.getValue() != null) {
					val2 = Long.toString(att.getIntValue());
				} else if (att.getType() == GUIAttribute.TYPE_DOUBLE && att.getValue() != null) {
					val2 = numberFormat.format(att.getDoubleValue());
				} else if (att.getType() == GUIAttribute.TYPE_DATE && att.getValue() != null) {
					val2 = dateFormat.format(att.getDateValue());
				}

			DiffRecord record = new DiffRecord(name, val1, val2);
			records.add(record);
		}
	}

	public class DiffRecord extends ListGridRecord {

		public DiffRecord(String name, String val1, String val2) {
			super();
			setName(name);
			setVal1(val1);
			setVal2(val2);
			if (isDifferent()) {
				setName("<b class='diff'>" + getAttribute("name") + "</b>");
				setVal1("<b class='diff'>" + getAttribute("val1") + "</b>");
				setVal2("<b class='diff'>" + getAttribute("val2") + "</b>");
			}
		}

		public void setName(String name) {
			setAttribute("name", name != null ? name : "");
		}

		public void setVal1(String val1) {
			setAttribute("val1", val1 != null ? val1 : "");
		}

		public void setVal2(String val2) {
			setAttribute("val2", val2 != null ? val2 : "");
		}

		public boolean isDifferent() {
			return !getAttributeAsString("val1").equals(getAttributeAsString("val2"));
		}
	}
}