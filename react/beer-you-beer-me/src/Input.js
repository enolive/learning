import React from "react";
import * as PropTypes from "prop-types";
import "./Input.css"

export function Input(props) {
    return (
        <div className="Input-sliderContainer">
            <p>
                <input className="Input-slider"
                       type="range"
                       min={0}
                       max={100}
                       value={props.value}
                       onChange={props.onChange}/>
            </p>
            <p>{props.value}</p>
        </div>
    );
}

Input.propTypes = {
    value: PropTypes.number,
    onChange: PropTypes.func
};