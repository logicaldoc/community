package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.TabBarControls;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * A tabset capable of showing a save widget. Useful for build up details panels
 * with editing capabilities
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class EditingTabSet extends TabSet {

	private HLayout savePanel = null;

	private Button saveButton;

	private Button cancelButton;

	public EditingTabSet(ClickHandler saveHandler, ClickHandler cancelHandler) {
		saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(1);
		saveButton.addClickHandler(saveHandler);
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		cancelButton = new Button(I18N.message("cancel"));
		cancelButton.setAutoFit(true);
		cancelButton.setMargin(1);
		cancelButton.setLayoutAlign(VerticalAlignment.CENTER);
		cancelButton.addClickHandler(event -> hideSave());
		if (cancelHandler != null)
			cancelButton.addClickHandler(cancelHandler);

		savePanel = new HLayout();
		savePanel.setHeight100();
		savePanel.setPadding(2);
		savePanel.setAlign(VerticalAlignment.CENTER);
		savePanel.setStyleName("warn");

		savePanel.setMembers(saveButton, cancelButton);

		savePanel.hide();

		setTabBarControls(savePanel, TabBarControls.TAB_SCROLLER, TabBarControls.TAB_PICKER);
	}

	public void displaySave() {
		if (saveButton != null)
			saveButton.setDisabled(false);
		if (cancelButton != null)
			cancelButton.setDisabled(false);
		setStyleName("warn");
		savePanel.show();
	}

	public void hideSave() {
		setStyleName(null);
		savePanel.hide();
	}

	public void disableSave() {
		if (saveButton != null)
			saveButton.setDisabled(true);
		if (cancelButton != null)
			cancelButton.setDisabled(false);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((savePanel == null) ? 0 : savePanel.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		EditingTabSet other = (EditingTabSet) obj;
		if (savePanel == null) {
			if (other.savePanel != null)
				return false;
		} else if (!savePanel.equals(other.savePanel))
			return false;
		return true;
	}
}