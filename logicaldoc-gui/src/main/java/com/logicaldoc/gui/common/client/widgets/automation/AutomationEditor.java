package com.logicaldoc.gui.common.client.widgets.automation;

import org.wisepersist.gwt.ace.client.AceEditor;
import org.wisepersist.gwt.ace.client.AceEditorMode;
import org.wisepersist.gwt.ace.client.AceEditorTheme;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VStack;

/**
 * Editor for automation scripts
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class AutomationEditor extends VStack {

	private AceEditor editor = new AceEditor();

	private String text;

	private String title;

	private ChangedHandler changedHandler;

	public AutomationEditor(String text, String title) {
		this(text, title, null);
	}

	public AutomationEditor(String text, String title, ChangedHandler changedHandler) {
		super();
		this.text = text;
		this.title = title;
		this.changedHandler = changedHandler;
	}

	@Override
	public void onDraw() {
		if (title != null) {
			Label label = new Label(I18N.message(title));
			label.setHeight(20);
			addMember(label);
		}
		addMember(editor);

		editor.startEditor();
		editor.setMode(AceEditorMode.VELOCITY);
		editor.setTheme(AceEditorTheme.ECLIPSE);
		editor.setShowGutter(true);
		editor.setShowPrintMargin(false);
		editor.setAutocompleteEnabled(true);

		AceEditor.addCompletionProvider(new AutomationCompletionProvider());

		editor.setWidth(getWidth() - 20 + "px");
		editor.setHeight(getHeight() - 20 + "px");

		if (text != null)
			editor.setText(text);

		editor.addOnChangeHandler(obj -> {
			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
	}

	@Override
	public void destroy() {
		editor.destroy();
		super.destroy();
	}

	@Override
	public Canvas setDisabled(boolean disabled) {
		editor.setVisible(!disabled);
		return super.setDisabled(disabled);
	}

	@Override
	public void setVisible(boolean visible) {
		editor.setVisible(visible);
		super.setVisible(visible);
	}

	public String getText() {
		return editor.getText();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}