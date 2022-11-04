import { h, render } from 'preact';
import App, { Report, Result } from './App';

const root = document.getElementById('root');

const report: Report<Result> =
    // @ts-ignore
    window.report;

render(<App report={report} />, root!);
