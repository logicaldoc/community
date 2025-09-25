package com.logicaldoc.gui.frontend.client.ai.robot;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * A GUI bean representing a Robot
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUIRobot implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private boolean enabled = true;

	private long classifierId;

	private long tokensDetectorId;

	private String avatar = """
			data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA2NDAgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuNy4yIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDI0IEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNMzIwIDBjNC40IDAgOCAzLjYgOCA4bDAgODggMTIwIDBjNTMgMCA5NiA0MyA5NiA5NmwwIDIyNGMwIDUzLTQzIDk2LTk2IDk2bC0yNTYgMGMtNTMgMC05Ni00My05Ni05NmwwLTIyNGMwLTUzIDQzLTk2IDk2LTk2bDEyMCAwIDAtODhjMC00LjQgMy42LTggOC04ek0xOTIgMTEyYy00NC4yIDAtODAgMzUuOC04MCA4MGwwIDIyNGMwIDQ0LjIgMzUuOCA4MCA4MCA4MGwyNTYgMGM0NC4yIDAgODAtMzUuOCA4MC04MGwwLTIyNGMwLTQ0LjItMzUuOC04MC04MC04MGwtMTI4IDAtMTI4IDB6bTggMjg4bDQ4IDBjNC40IDAgOCAzLjYgOCA4cy0zLjYgOC04IDhsLTQ4IDBjLTQuNCAwLTgtMy42LTgtOHMzLjYtOCA4LTh6bTk2IDBsNDggMGM0LjQgMCA4IDMuNiA4IDhzLTMuNiA4LTggOGwtNDggMGMtNC40IDAtOC0zLjYtOC04czMuNi04IDgtOHptOTYgMGw0OCAwYzQuNCAwIDggMy42IDggOHMtMy42IDgtOCA4bC00OCAwYy00LjQgMC04LTMuNi04LThzMy42LTggOC04ek0yMjQgMjE2YTQwIDQwIDAgMSAwIDAgODAgNDAgNDAgMCAxIDAgMC04MHptNTYgNDBhNTYgNTYgMCAxIDEgLTExMiAwIDU2IDU2IDAgMSAxIDExMiAwem05NiAwYTQwIDQwIDAgMSAwIDgwIDAgNDAgNDAgMCAxIDAgLTgwIDB6bTQwIDU2YTU2IDU2IDAgMSAxIDAtMTEyIDU2IDU2IDAgMSAxIDAgMTEyek00OCAyMjRsMTYgMCAwIDE2LTE2IDBjLTE3LjcgMC0zMiAxNC4zLTMyIDMybDAgOTZjMCAxNy43IDE0LjMgMzIgMzIgMzJsMTYgMCAwIDE2LTE2IDBjLTI2LjUgMC00OC0yMS41LTQ4LTQ4bDAtOTZjMC0yNi41IDIxLjUtNDggNDgtNDh6TTU5MiA0MDBjMTcuNyAwIDMyLTE0LjMgMzItMzJsMC05NmMwLTE3LjctMTQuMy0zMi0zMi0zMmwtMTYgMCAwLTE2IDE2IDBjMjYuNSAwIDQ4IDIxLjUgNDggNDhsMCA5NmMwIDI2LjUtMjEuNSA0OC00OCA0OGwtMTYgMCAwLTE2IDE2IDB6Ii8+PC9zdmc+
			""";
	/**
	 * Map of response scripts: code is the category name, value is the
	 * automation script
	 */
	private List<GUIValue> answers = new ArrayList<>();

	public GUIRobot(long id, String name) {
		super();
		this.id = id;
		this.name = name;
		answers.add(new GUIValue("UNKNOWN", "$I18N.get('cannotunderstand')"));
	}

	public GUIRobot() {
		super();
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public long getClassifierId() {
		return classifierId;
	}

	public void setClassifierId(long classifierId) {
		this.classifierId = classifierId;
	}

	public long getTokensDetectorId() {
		return tokensDetectorId;
	}

	public void setTokensDetectorId(long tokensDetectorId) {
		this.tokensDetectorId = tokensDetectorId;
	}

	public List<GUIValue> getAnswers() {
		return answers;
	}

	public void setAnswers(List<GUIValue> answers) {
		this.answers = answers;
	}
	
	public String getAvatar() {
		return avatar;
	}

	public void setAvatar(String avatar) {
		this.avatar = avatar;
	}
}