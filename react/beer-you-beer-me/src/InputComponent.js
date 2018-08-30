import React, {Component} from "react";
import * as PropTypes from "prop-types";

export class InputComponent extends Component {
    render() {
        return (
            <div>
                <input type="range" min={0} max={100} value={this.props.value} onChange={this.props.onChange}/>
                <p className="App-intro">
                    {this.props.value}
                </p>
            </div>
        );
    }
}

InputComponent.propTypes = {
    value: PropTypes.number,
    onChange: PropTypes.func
};