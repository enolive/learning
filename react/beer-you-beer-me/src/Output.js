import React from "react";
import * as PropTypes from "prop-types";
import "./Output.css";

export function Output(props) {
    return (
        <div className="Output-song">
            <p>{props.songText}</p>
        </div>
    );
}

Output.propTypes = {songText: PropTypes.any};