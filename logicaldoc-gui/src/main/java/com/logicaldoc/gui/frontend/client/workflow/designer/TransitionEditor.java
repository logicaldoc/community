package com.logicaldoc.gui.frontend.client.workflow.designer;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used for the workflow transition settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class TransitionEditor extends Window {

	private StateWidget widget;

	public TransitionEditor(StateWidget widget) {
		this.widget = widget;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);

		GUITransition transition = widget.getTransition();
		setTitle(I18N.message("editworkflowstate", I18N.message("transition")) + " - " + transition.getText());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(600);
		setAutoSize(true);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		GUITransition transition = widget.getTransition();

		TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", "execscriptontranschosen",
				transition.getOnChosen(), null, false);
		automation.setWidth("*");
		automation.setHeight(200);
		automation.setColSpan(2);
		automation.setWrapTitle(false);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		final TextItem name = ItemFactory.newTextItem("name", transition.getText());
		name.setRequired(true);

		RadioGroupItem requiresNote = ItemFactory.newBooleanSelector("requiresNote", "requiresnote");
		requiresNote.setWrapTitle(false);
		requiresNote.setDefaultValue(transition.isRequiresNote() ? "yes" : "no");

		SpinnerItem minNoteSize = ItemFactory.newSpinnerItem("minnotesize",
				transition.getMinNoteSize() != null && transition.getMinNoteSize() > 0 ? transition.getMinNoteSize()
						: null);
		minNoteSize.setMin(0);
		minNoteSize.setStep(10);
		if (Session.get().getConfig("gui.note.maxlength") != null)
			minNoteSize.setMax(Session.get().getConfigAsInt("gui.note.maxlength"));
		minNoteSize.setHint(I18N.message("chars").toLowerCase());
		minNoteSize.setWrapTitle(false);

		final DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		form.setWidth100();
		form.setHeight100();
		form.setItems(requiresNote, minNoteSize, automation);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(click -> {
			if (Boolean.TRUE.equals(name.validate())) {
				String transitionName = name.getValue().toString().trim().replace("'", "");

				// Remove the ' because of the WF engine would go in error
				// saving into the DB
				transition.setText(transitionName);
				TransitionEditor.this.widget.setContents(transitionName);
				transition.setOnChosen(form.getValueAsString("automation"));
				transition.setRequiresNote("yes".equals(form.getValueAsString("requiresNote")));
				transition.setMinNoteSize((Integer) form.getValue("minnotesize"));

				destroy();
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(click -> destroy());

		toolStrip.addFormItem(name);
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(form);

		reflowNow();
	}
}