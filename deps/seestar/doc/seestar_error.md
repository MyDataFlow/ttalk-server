

# Module seestar_error #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-error">error()</a> ###


__abstract datatype__: `error()`




### <a name="type-write_type">write_type()</a> ###



<pre><code>
write_type() = simple | batch | unlogged_batch | counter | batch_log
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alive-1">alive/1</a></td><td></td></tr><tr><td valign="top"><a href="#code-1">code/1</a></td><td></td></tr><tr><td valign="top"><a href="#consistency-1">consistency/1</a></td><td></td></tr><tr><td valign="top"><a href="#data_present-1">data_present/1</a></td><td></td></tr><tr><td valign="top"><a href="#keyspace-1">keyspace/1</a></td><td></td></tr><tr><td valign="top"><a href="#message-1">message/1</a></td><td></td></tr><tr><td valign="top"><a href="#query_id-1">query_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#received-1">received/1</a></td><td></td></tr><tr><td valign="top"><a href="#required-1">required/1</a></td><td></td></tr><tr><td valign="top"><a href="#table-1">table/1</a></td><td></td></tr><tr><td valign="top"><a href="#write_type-1">write_type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alive-1"></a>

### alive/1 ###


<pre><code>
alive(Error::<a href="#type-error">error()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="code-1"></a>

### code/1 ###


<pre><code>
code(Error::<a href="#type-error">error()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="consistency-1"></a>

### consistency/1 ###


<pre><code>
consistency(Error::<a href="#type-error">error()</a>) -&gt; <a href="seestar.md#type-consistency">seestar:consistency()</a>
</code></pre>

<br></br>



<a name="data_present-1"></a>

### data_present/1 ###


<pre><code>
data_present(Error::<a href="#type-error">error()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="keyspace-1"></a>

### keyspace/1 ###


<pre><code>
keyspace(Error::<a href="#type-error">error()</a>) -&gt; binary()
</code></pre>

<br></br>



<a name="message-1"></a>

### message/1 ###


<pre><code>
message(Error::<a href="#type-error">error()</a>) -&gt; binary()
</code></pre>

<br></br>



<a name="query_id-1"></a>

### query_id/1 ###


<pre><code>
query_id(Error::<a href="#type-error">error()</a>) -&gt; binary()
</code></pre>

<br></br>



<a name="received-1"></a>

### received/1 ###


<pre><code>
received(Error::<a href="#type-error">error()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="required-1"></a>

### required/1 ###


<pre><code>
required(Error::<a href="#type-error">error()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="table-1"></a>

### table/1 ###


<pre><code>
table(Error::<a href="#type-error">error()</a>) -&gt; binary() | undefined
</code></pre>

<br></br>



<a name="write_type-1"></a>

### write_type/1 ###


<pre><code>
write_type(Error::<a href="#type-error">error()</a>) -&gt; <a href="#type-write_type">write_type()</a>
</code></pre>

<br></br>



