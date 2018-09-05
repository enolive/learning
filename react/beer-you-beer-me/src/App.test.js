import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import {configure, shallow} from "enzyme";
import Adapter from 'enzyme-adapter-react-16';
import renderer from 'react-test-renderer';
import seedrandom from "seedrandom";

configure({adapter: new Adapter()});

describe('App.js', () => {
    it('renders without crashing', () => {
        const div = document.createElement('div');
        ReactDOM.render(<App/>, div);
        ReactDOM.unmountComponentAtNode(div);
    });
    it('has expected state', () => {
        const wrapper = shallow(<App/>);
        expect(wrapper.state().sliderValue).toEqual(50);
        expect(wrapper.state().songText).toContain('Beer');
    });
    it('matches snapshot', () => {
        const tree = renderer.create(<App rng={seedrandom('Test')}/>).toJSON();
        expect(tree).toMatchSnapshot();
    });
});